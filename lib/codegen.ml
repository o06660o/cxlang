exception Codegen_error of string

let error fmt = Printf.ksprintf (fun msg -> raise (Codegen_error msg)) fmt

(**************************************************************************************************)

type ctx = {
  llctx : Llvm.llcontext;
  llmod : Llvm.llmodule;
  llbdr : Llvm.llbuilder;
  mutable types : (Ast.id, Ast.ty) Hashtbl.t list;
  mutable vars : (Ast.id, Ast.ty * Llvm.lltype * Llvm.llvalue) Hashtbl.t list;
  mutable break_blks : Llvm.llbasicblock list;
  mutable continue_blks : Llvm.llbasicblock list;
}

let ctx_create (llctx : Llvm.llcontext) : ctx =
  let llmod = Llvm.create_module llctx "" in
  let llbdr = Llvm.builder llctx in
  {
    llctx;
    llmod;
    llbdr;
    types = [ Hashtbl.create 16 ];
    vars = [ Hashtbl.create 16 ];
    break_blks = [];
    continue_blks = [];
  }

(**************************************************************************************************)

let resolve_ty (ty : Ast.ty) (ctx : ctx) : Ast.ty =
  let rec loop (vis : string list) (ty : Ast.ty) : Ast.ty =
    match ty with
    | Ast.Alias id -> (
        if List.mem id vis then error "cyclic type alias \"%s\"" id;
        match List.find_map (fun types -> Hashtbl.find_opt types id) ctx.types with
        | Some ty -> loop (id :: vis) ty
        | None -> error "undefined type alias \"%s\"" id)
    | Ast.Array (ty, cnt) -> Ast.Array (loop vis ty, cnt)
    | Ast.Tuple items -> Ast.Tuple (List.map (fun ty -> loop vis ty) items)
    | Ast.Struct items -> Ast.Struct (List.map (fun (id, ty) -> (id, loop vis ty)) items)
    | Ast.Func (ptys, rty) -> Ast.Func (List.map (loop vis) ptys, Option.map (loop vis) rty)
    | _ -> ty
  in
  loop [] ty

let rec emit_ty (ty : Ast.ty) (ctx : ctx) : Llvm.lltype =
  match ty with
  | Ast.Bool -> Llvm.i1_type ctx.llctx
  | Ast.Int (bits, _) -> Llvm.integer_type ctx.llctx bits
  | Ast.Array (ty, cnt) -> Llvm.array_type (emit_ty ty ctx) cnt
  | Ast.Tuple items ->
      Llvm.struct_type ctx.llctx (Array.of_list (List.map (fun ty -> emit_ty ty ctx) items))
  | Ast.Struct items ->
      Llvm.struct_type ctx.llctx (Array.of_list (List.map (fun (_, ty) -> emit_ty ty ctx) items))
  | Ast.Ptr -> Llvm.pointer_type ctx.llctx
  | Ast.Func _ -> Llvm.pointer_type ctx.llctx
  | Ast.Alias id -> emit_ty (resolve_ty ty ctx) ctx

let rec emit_expr (expr : Ast.expr) (ctx : ctx) : Ast.ty * Llvm.lltype * Llvm.llvalue =
  let rec addreval (expr : Ast.expr) (ctx : ctx) : Ast.ty * Llvm.lltype * Llvm.llvalue =
    match expr with
    | Ast.Id id -> (
        match List.find_map (fun vars -> Hashtbl.find_opt vars id) ctx.vars with
        | Some (ty, llty, addr) -> (ty, llty, addr)
        | None -> error "undefined variable \"%s\"" id)
    | Ast.MemArray (base, idx) -> (
        let bty, bllty, baddr = addreval base ctx in
        match resolve_ty bty ctx with
        | Ast.Array (ety, _) ->
            let _, _, idx = emit_expr idx ctx in
            let zero = Llvm.const_int (Llvm.i32_type ctx.llctx) 0 in
            let addr = Llvm.build_gep bllty baddr [| zero; idx |] "" ctx.llbdr in
            (ety, emit_ty ety ctx, addr)
        | _ -> error "indexing requires an array")
    | Ast.MemTuple (base, loc) -> (
        let bty, bllty, baddr = addreval base ctx in
        match resolve_ty bty ctx with
        | Ast.Tuple items ->
            let ity = List.nth items loc in
            let addr = Llvm.build_struct_gep bllty baddr loc "" ctx.llbdr in
            (ity, emit_ty ity ctx, addr)
        | _ -> error "member access requires an aggregate")
    | Ast.MemStruct (base, loc) -> (
        let bty, bllty, baddr = addreval base ctx in
        match resolve_ty bty ctx with
        | Ast.Struct items -> (
            match
              List.filter
                (fun (_, name, _) -> name = loc)
                (List.mapi (fun idx (name, ty) -> (idx, name, ty)) items)
            with
            | [ (idx, _, ty) ] ->
                let addr = Llvm.build_struct_gep bllty baddr idx "" ctx.llbdr in
                (ty, emit_ty ty ctx, addr)
            | [] -> error "struct has no field \"%s\"" loc
            | _ -> error "ambiguous struct field \"%s\"" loc)
        | _ -> error "member access requires a struct")
    | Ast.Deref (ptr, ty) -> (
        let pty, _, pllval = emit_expr ptr ctx in
        match resolve_ty pty ctx with
        | Ast.Ptr -> (ty, emit_ty ty ctx, pllval)
        | _ -> error "dereference requires a pointer")
    | _ -> error "expression is not an lvalue"
  in
  match expr with
  | Ast.NewTrue ->
      let ty = Ast.Bool in
      let llty = emit_ty ty ctx in
      (ty, llty, Llvm.const_int llty 1)
  | Ast.NewFalse ->
      let ty = Ast.Bool in
      let llty = emit_ty ty ctx in
      (ty, llty, Llvm.const_int llty 0)
  | Ast.NewInt literal ->
      let digits, ty =
        match String.split_on_char '_' literal with
        | [ digits; suffix ] when String.length suffix >= 2 -> (
            let bits = int_of_string (String.sub suffix 1 (String.length suffix - 1)) in
            if bits mod 8 <> 0 then error "integer bit width must be a multiple of 8, got %d" bits;
            match suffix.[0] with
            | 'i' -> (digits, Ast.Int (bits, true))
            | 'u' -> (digits, Ast.Int (bits, false))
            | _ -> error "invalid integer suffix %S" suffix)
        | _ -> error "invalid integer literal %S" literal
      in
      let llty = emit_ty ty ctx in
      let llval = Llvm.const_int_of_string llty digits 10 in
      (ty, llty, llval)
  | Ast.NewChar value ->
      let ty = Ast.Int (8, false) in
      let llty = emit_ty ty ctx in
      (ty, llty, Llvm.const_int llty value)
  | Ast.NewString bytes ->
      let ty = Ast.Array (Ast.Int (8, false), String.length bytes) in
      (ty, emit_ty ty ctx, Llvm.const_string ctx.llctx bytes)
  | Ast.NewArray items ->
      let items = List.map (fun expr -> emit_expr expr ctx) items in
      let ity =
        match items with
        | [] -> error "new empty array not allowed"
        | (ty, _, _) :: tl ->
            List.iter
              (fun (ity, _, _) ->
                if resolve_ty ity ctx <> resolve_ty ty ctx then
                  error "array constant has mixed element types")
              tl;
            ty
      in
      let ty = Ast.Array (ity, List.length items) in
      let llty = emit_ty ty ctx in
      let elems = List.mapi (fun index (_, _, value) -> (index, value)) items in
      let llval =
        List.fold_left
          (fun array (index, value) -> Llvm.build_insertvalue array value index "" ctx.llbdr)
          (Llvm.undef llty) elems
      in
      (ty, llty, llval)
  | Ast.NewTuple exprs ->
      let items = List.map (fun expr -> emit_expr expr ctx) exprs in
      let ty = Ast.Tuple (List.map (fun (ty, _, _) -> ty) items) in
      let llty = emit_ty ty ctx in
      let indexed = List.mapi (fun index (_, _, value) -> (index, value)) items in
      let llval =
        List.fold_left
          (fun tuple (index, value) -> Llvm.build_insertvalue tuple value index "" ctx.llbdr)
          (Llvm.undef llty) indexed
      in
      (ty, llty, llval)
  | Ast.NewStruct items ->
      let items =
        List.map
          (fun (name, expr) ->
            let ty, _, llval = emit_expr expr ctx in
            (name, ty, llval))
          items
      in
      let ty = Ast.Struct (List.map (fun (name, ty, _) -> (name, ty)) items) in
      let llty = emit_ty ty ctx in
      let llval =
        List.fold_left
          (fun acc (idx, llval) -> Llvm.build_insertvalue acc llval idx "" ctx.llbdr)
          (Llvm.undef llty)
          (List.mapi (fun idx (_, _, llval) -> (idx, llval)) items)
      in
      (ty, llty, llval)
  | Ast.NewFunc (params, rty, body) ->
      let ptys = List.map (fun (_, ty) -> resolve_ty ty ctx) params in
      let rty = Option.map (fun ty -> resolve_ty ty ctx) rty in
      let ty = Ast.Func (ptys, rty) in
      let llptys = Array.of_list (List.map (fun ty -> emit_ty ty ctx) ptys) in
      let llrty = match rty with None -> Llvm.void_type ctx.llctx | Some ty -> emit_ty ty ctx in
      let llfty = Llvm.function_type llrty llptys in
      let globals =
        match List.rev ctx.vars with
        | globals :: _ -> globals
        | [] -> error "internal: unexpected empty `ctx.vars`"
      in
      let llfn = Llvm.define_function "" llfty ctx.llmod in
      Llvm.set_linkage Llvm.Linkage.Internal llfn;

      let vars = Hashtbl.create 16 in
      let fctx =
        {
          ctx with
          llbdr = Llvm.builder_at_end ctx.llctx (Llvm.entry_block llfn);
          vars = [ vars; globals ];
          break_blks = [];
          continue_blks = [];
        }
      in
      List.iter2
        (fun (id, pty) llpval ->
          if Hashtbl.mem vars id then error "duplicate parameter \"%s\"" id;
          let llpty = emit_ty pty fctx in
          let addr = Llvm.build_alloca llpty id fctx.llbdr in
          ignore (Llvm.build_store llpval addr fctx.llbdr);
          Hashtbl.add vars id (pty, llpty, addr))
        params
        (Array.to_list (Llvm.params llfn));
      let terminated = emit_block body fctx in
      (if not terminated then
         match rty with
         | None -> ignore (Llvm.build_ret_void fctx.llbdr)
         | Some _ -> error "control reaches end of non-void lambda");
      (ty, emit_ty ty ctx, llfn)
  | Ast.Assn (var, expr) ->
      let ty, llty, addr = addreval var ctx in
      let ety, _, llval = emit_expr expr ctx in
      if resolve_ty ty ctx <> resolve_ty ety ctx then error "assignment type mismatch";
      ignore (Llvm.build_store llval addr ctx.llbdr);
      (ty, llty, llval)
  | Ast.Unary (op, expr) -> (
      let ty, llty, llval = emit_expr expr ctx in
      (match resolve_ty ty ctx with
      | Ast.Bool | Ast.Int _ -> ()
      | _ -> error "unary operator requires scalar operands");
      match op with
      | Ast.Not -> (ty, llty, Llvm.build_not llval "" ctx.llbdr)
      | Ast.LgNot ->
          let llval = Llvm.build_icmp Llvm.Icmp.Eq llval (Llvm.const_null llty) "" ctx.llbdr in
          let ty = Ast.Bool in
          (ty, emit_ty ty ctx, llval))
  | Ast.Binary (lhs, ((Ast.LgAnd | Ast.LgOr) as op), rhs) ->
      let lty, llty, lval = emit_expr lhs ctx in
      (match resolve_ty lty ctx with
      | Ast.Bool | Ast.Int _ -> ()
      | _ -> error "binary operator requires scalar operands");
      let bool_ty = Ast.Bool in
      let bool_llty = emit_ty bool_ty ctx in
      let llfn = Llvm.block_parent (Llvm.insertion_block ctx.llbdr) in
      let rhs_blk = Llvm.append_block ctx.llctx "" llfn in
      let join_blk = Llvm.append_block ctx.llctx "" llfn in

      let cond = Llvm.build_icmp Llvm.Icmp.Ne lval (Llvm.const_null llty) "" ctx.llbdr in
      let lhs_end = Llvm.insertion_block ctx.llbdr in
      let short_val, true_blk, false_blk =
        match op with
        | Ast.LgAnd -> (Llvm.const_int bool_llty 0, rhs_blk, join_blk)
        | Ast.LgOr -> (Llvm.const_int bool_llty 1, join_blk, rhs_blk)
        | _ -> assert false
      in
      ignore (Llvm.build_cond_br cond true_blk false_blk ctx.llbdr);

      Llvm.position_at_end rhs_blk ctx.llbdr;
      let rty, rllty, rval = emit_expr rhs ctx in
      if resolve_ty lty ctx <> resolve_ty rty ctx then error "binary operand type mismatch";
      let rcond = Llvm.build_icmp Llvm.Icmp.Ne rval (Llvm.const_null rllty) "" ctx.llbdr in
      let rhs_end = Llvm.insertion_block ctx.llbdr in
      ignore (Llvm.build_br join_blk ctx.llbdr);

      Llvm.position_at_end join_blk ctx.llbdr;
      let result = Llvm.build_phi [ (short_val, lhs_end); (rcond, rhs_end) ] "" ctx.llbdr in
      (bool_ty, bool_llty, result)
  | Ast.Binary (lhs, op, rhs) -> (
      let lty, llty, lval = emit_expr lhs ctx in
      let rty, _, rval = emit_expr rhs ctx in
      if resolve_ty lty ctx <> resolve_ty rty ctx then error "binary operand type mismatch";
      let sty =
        match resolve_ty lty ctx with
        | (Ast.Bool | Ast.Int _) as sty -> sty
        | _ -> error "binary operator requires scalar operands"
      in
      let signed = match sty with Ast.Int (_, true) -> true | _ -> false in
      let bool_ty = Ast.Bool in
      let bool_llty = emit_ty bool_ty ctx in
      match op with
      | Ast.Div ->
          let llval =
            if signed then Llvm.build_sdiv lval rval "" ctx.llbdr
            else Llvm.build_udiv lval rval "" ctx.llbdr
          in
          (lty, llty, llval)
      | Ast.Mul -> (lty, llty, Llvm.build_mul lval rval "" ctx.llbdr)
      | Ast.Rem ->
          let llval =
            if signed then Llvm.build_srem lval rval "" ctx.llbdr
            else Llvm.build_urem lval rval "" ctx.llbdr
          in
          (lty, llty, llval)
      | Ast.Add -> (lty, llty, Llvm.build_add lval rval "" ctx.llbdr)
      | Ast.Sub -> (lty, llty, Llvm.build_sub lval rval "" ctx.llbdr)
      | Ast.Shl -> (lty, llty, Llvm.build_shl lval rval "" ctx.llbdr)
      | Ast.Shr ->
          let llval =
            if signed then Llvm.build_ashr lval rval "" ctx.llbdr
            else Llvm.build_lshr lval rval "" ctx.llbdr
          in
          (lty, llty, llval)
      | Ast.Lt ->
          let pred = if signed then Llvm.Icmp.Slt else Llvm.Icmp.Ult in
          let llval = Llvm.build_icmp pred lval rval "" ctx.llbdr in
          (bool_ty, bool_llty, llval)
      | Ast.Le ->
          let pred = if signed then Llvm.Icmp.Sle else Llvm.Icmp.Ule in
          let llval = Llvm.build_icmp pred lval rval "" ctx.llbdr in
          (bool_ty, bool_llty, llval)
      | Ast.Gt ->
          let pred = if signed then Llvm.Icmp.Sgt else Llvm.Icmp.Ugt in
          let llval = Llvm.build_icmp pred lval rval "" ctx.llbdr in
          (bool_ty, bool_llty, llval)
      | Ast.Ge ->
          let pred = if signed then Llvm.Icmp.Sge else Llvm.Icmp.Uge in
          let llval = Llvm.build_icmp pred lval rval "" ctx.llbdr in
          (bool_ty, bool_llty, llval)
      | Ast.Eq -> (bool_ty, bool_llty, Llvm.build_icmp Llvm.Icmp.Eq lval rval "" ctx.llbdr)
      | Ast.Ne -> (bool_ty, bool_llty, Llvm.build_icmp Llvm.Icmp.Ne lval rval "" ctx.llbdr)
      | Ast.And -> (lty, llty, Llvm.build_and lval rval "" ctx.llbdr)
      | Ast.Xor -> (lty, llty, Llvm.build_xor lval rval "" ctx.llbdr)
      | Ast.Or -> (lty, llty, Llvm.build_or lval rval "" ctx.llbdr)
      | Ast.LgAnd | Ast.LgOr -> assert false)
  | Ast.Cast (ty, expr) ->
      let srcty, srcllty, srcllval = emit_expr expr ctx in
      let srcsty =
        match resolve_ty srcty ctx with
        | (Ast.Bool | Ast.Int _) as sty -> sty
        | _ -> error "cast requires a scalar operand"
      in
      let sty =
        match resolve_ty ty ctx with
        | (Ast.Bool | Ast.Int _) as sty -> sty
        | _ -> error "cast requires a scalar target"
      in
      let llty = emit_ty ty ctx in
      let llval =
        match (srcsty, sty) with
        | Ast.Bool, Ast.Bool -> srcllval
        | Ast.Bool, Ast.Int _ -> Llvm.build_zext srcllval llty "" ctx.llbdr
        | Ast.Int _, Ast.Bool ->
            Llvm.build_icmp Llvm.Icmp.Ne srcllval (Llvm.const_null srcllty) "" ctx.llbdr
        | Ast.Int (_, signed), Ast.Int _ ->
            let sbits = Llvm.integer_bitwidth srcllty in
            let bits = Llvm.integer_bitwidth llty in
            if sbits = bits then srcllval
            else if sbits > bits then Llvm.build_trunc srcllval llty "" ctx.llbdr
            else if signed then Llvm.build_sext srcllval llty "" ctx.llbdr
            else Llvm.build_zext srcllval llty "" ctx.llbdr
        | _ -> assert false
      in
      (ty, llty, llval)
  | Ast.Addrof expr ->
      let _, _, addr = addreval expr ctx in
      let ty = Ast.Ptr in
      (ty, emit_ty ty ctx, addr)
  | Ast.Call (expr, args) -> (
      match emit_call expr args ctx with
      | Some resp -> resp
      | None -> error "void function used as a value")
  | Ast.Id _ | Ast.MemArray _ | Ast.MemTuple _ | Ast.MemStruct _ | Ast.Deref _ ->
      let ty, llty, addr = addreval expr ctx in
      let llval = Llvm.build_load llty addr "" ctx.llbdr in
      (ty, llty, llval)

and emit_call (expr : Ast.expr) (args : Ast.expr list) (ctx : ctx) :
    (Ast.ty * Llvm.lltype * Llvm.llvalue) option =
  let fty, _, llfn = emit_expr expr ctx in
  let ptys, rty =
    match resolve_ty fty ctx with
    | Ast.Func (ptys, rty) -> (ptys, rty)
    | _ -> error "call requires a function"
  in
  if List.length ptys <> List.length args then error "argument count mismatch";
  let llargs =
    Array.of_list
      (List.rev
         (List.fold_left2
            (fun acc pty arg ->
              let aty, _, llarg = emit_expr arg ctx in
              if resolve_ty aty ctx <> pty then error "argument type mismatch";
              llarg :: acc)
            [] ptys args))
  in
  let llptys = Array.of_list (List.map (fun ty -> emit_ty ty ctx) ptys) in
  let llrty = match rty with None -> Llvm.void_type ctx.llctx | Some ty -> emit_ty ty ctx in
  let llfty = Llvm.function_type llrty llptys in
  let llcall = Llvm.build_call llfty llfn llargs "" ctx.llbdr in
  match rty with Some ty -> Some (ty, llrty, llcall) | None -> None

and emit_stmt (stmt : Ast.stmt) (ctx : ctx) : bool =
  match stmt with
  | Ast.Decl decl ->
      emit_decl decl ctx;
      false
  | Ast.Expr (Ast.Call (expr, args)) ->
      ignore (emit_call expr args ctx);
      false
  | Ast.Expr expr ->
      ignore (emit_expr expr ctx);
      false
  | Ast.If (cond, then_stmts, else_stmts) ->
      let llfn = Llvm.block_parent (Llvm.insertion_block ctx.llbdr) in
      let then_blk = Llvm.append_block ctx.llctx "" llfn in
      let else_blk = Llvm.append_block ctx.llctx "" llfn in
      let join_blk = Llvm.append_block ctx.llctx "" llfn in

      let _, _, llcval = emit_expr cond ctx in
      ignore (Llvm.build_cond_br llcval then_blk else_blk ctx.llbdr);

      Llvm.position_at_end then_blk ctx.llbdr;
      let then_terminated = emit_block then_stmts ctx in
      if not then_terminated then ignore (Llvm.build_br join_blk ctx.llbdr);

      Llvm.position_at_end else_blk ctx.llbdr;
      let else_terminated = emit_block else_stmts ctx in
      if not else_terminated then ignore (Llvm.build_br join_blk ctx.llbdr);

      Llvm.position_at_end join_blk ctx.llbdr;
      false
  | Ast.Loop body ->
      let llfn = Llvm.block_parent (Llvm.insertion_block ctx.llbdr) in
      let body_blk = Llvm.append_block ctx.llctx "" llfn in
      let exit_blk = Llvm.append_block ctx.llctx "" llfn in

      ignore (Llvm.build_br body_blk ctx.llbdr);

      ctx.break_blks <- exit_blk :: ctx.break_blks;
      ctx.continue_blks <- body_blk :: ctx.continue_blks;

      Llvm.position_at_end body_blk ctx.llbdr;
      let terminated = emit_block body ctx in
      if not terminated then ignore (Llvm.build_br body_blk ctx.llbdr);

      (match ctx.continue_blks with
      | [] -> error "internal: unexpected empty `ctx.continue_blks`"
      | hd :: tl -> ctx.continue_blks <- tl);
      (match ctx.break_blks with
      | [] -> error "internal: unexpected empty `ctx.break_blks`"
      | hd :: tl -> ctx.break_blks <- tl);

      Llvm.position_at_end exit_blk ctx.llbdr;
      false
  | Ast.Break -> (
      match ctx.break_blks with
      | [] -> error "break outside loop"
      | blk :: _ ->
          ignore (Llvm.build_br blk ctx.llbdr);
          true)
  | Ast.Continue -> (
      match ctx.continue_blks with
      | [] -> error "continue outside loop"
      | blk :: _ ->
          ignore (Llvm.build_br blk ctx.llbdr);
          true)
  | Ast.Return expr -> (
      match expr with
      | Some expr ->
          let _, _, llval = emit_expr expr ctx in
          ignore (Llvm.build_ret llval ctx.llbdr);
          true
      | None ->
          ignore (Llvm.build_ret_void ctx.llbdr);
          true)

and emit_block (block : Ast.block) (ctx : ctx) : bool =
  ctx.vars <- Hashtbl.create 16 :: ctx.vars;
  ctx.types <- Hashtbl.create 16 :: ctx.types;
  let terminated = List.exists (fun stmt -> emit_stmt stmt ctx) block in
  (match ctx.types with
  | [] -> error "internal: unexpected empty `ctx.types`"
  | hd :: tl -> ctx.types <- tl);
  (match ctx.vars with
  | [] -> error "internal: unexpected empty `ctx.vars`"
  | hd :: tl -> ctx.vars <- tl);
  terminated

and emit_decl (decl : Ast.decl) (ctx : ctx) : unit =
  match decl with
  | Ast.Var (id, expr) ->
      let vars, is_global =
        match ctx.vars with
        | [ vars ] -> (vars, true)
        | vars :: _ -> (vars, false)
        | [] -> error "internal: unexpected empty `ctx.vars`"
      in
      if Hashtbl.mem vars id then error "duplicate variable declaration \"%s\"" id;
      (if is_global then
         match expr with
         | Ast.NewFunc _ -> ()
         | _ -> error "global declaration \"%s\" requires a lambda" id);
      let ty, llty, llval = emit_expr expr ctx in
      let addr =
        if is_global then Llvm.define_global id llval ctx.llmod
        else
          let addr = Llvm.build_alloca llty id ctx.llbdr in
          ignore (Llvm.build_store llval addr ctx.llbdr);
          addr
      in
      Hashtbl.add vars id (resolve_ty ty ctx, llty, addr)
  | Ast.Type (id, ty) -> (
      match ctx.types with
      | types :: _ ->
          if Hashtbl.mem types id then error "duplicate type declaration \"%s\"" id;
          Hashtbl.add types id ty
      | [] -> error "internal: unexpected empty `ctx.types`")

let codegen (prog : Ast.prog) (llctx : Llvm.llcontext) : Llvm.llmodule =
  let ctx = ctx_create llctx in
  List.iter (fun decl -> emit_decl decl ctx) prog;
  ctx.llmod
