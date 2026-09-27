exception Codegen_error of string

let error fmt = Printf.ksprintf (fun msg -> raise (Codegen_error msg)) fmt

(**************************************************************************************************)

type ctx = {
  llctx : Llvm.llcontext;
  llmod : Llvm.llmodule;
  llbdr : Llvm.llbuilder;
  funcs : (Ast.id, Ast.ty list * Ast.ty option * Llvm.lltype * Llvm.llvalue) Hashtbl.t;
  mutable vars : (Ast.id, Ast.ty * Llvm.lltype * Llvm.llvalue) Hashtbl.t list;
  mutable break_blks : Llvm.llbasicblock list;
  mutable continue_blks : Llvm.llbasicblock list;
}

let ctx_create (llctx : Llvm.llcontext) : ctx =
  let llmod = Llvm.create_module llctx "" in
  let llbdr = Llvm.builder llctx in
  { llctx; llmod; llbdr; funcs = Hashtbl.create 0; vars = []; break_blks = []; continue_blks = [] }

(**************************************************************************************************)

let rec emit_ty (ty : Ast.ty) (ctx : ctx) : Llvm.lltype =
  match ty with
  | Ast.Int Ast.Bool -> Llvm.i1_type ctx.llctx
  | Ast.Int (Signed bits) | Ast.Int (Unsigned bits) -> Llvm.integer_type ctx.llctx bits
  | Ast.Arr (ty, cnt) -> Llvm.array_type (emit_ty ty ctx) cnt
  | Ast.Aggr items ->
      Llvm.struct_type ctx.llctx (Array.of_list (List.map (fun ty -> emit_ty ty ctx) items))
  | Ast.Ptr -> Llvm.pointer_type ctx.llctx

let rec emit_expr (expr : Ast.expr) (ctx : ctx) : Ast.ty * Llvm.lltype * Llvm.llvalue =
  let rec consteval (expr : Ast.const) (ctx : ctx) : Ast.ty * Llvm.lltype * Llvm.llvalue =
    match expr with
    | Ast.ConstInt (("true" | "false") as literal) ->
        let ty = Ast.Int Ast.Bool in
        let llty = emit_ty ty ctx in
        let llval = Llvm.const_int llty (if literal = "false" then 0 else 1) in
        (ty, llty, llval)
    | Ast.ConstInt literal ->
        let digits, ty =
          match String.split_on_char '_' literal with
          | [ digits ] -> (digits, Ast.Int (Ast.Signed 32))
          | [ digits; suffix ] when String.length suffix >= 2 -> (
              let bits = int_of_string (String.sub suffix 1 (String.length suffix - 1)) in
              if bits mod 8 <> 0 then error "integer bit width must be a multiple of 8, got %d" bits;
              match suffix.[0] with
              | 'i' -> (digits, Ast.Int (Ast.Signed bits))
              | 'u' -> (digits, Ast.Int (Ast.Unsigned bits))
              | _ -> error "invalid integer suffix %S" suffix)
          | _ -> error "invalid integer literal %S" literal
        in
        let llty = emit_ty ty ctx in
        let llval = Llvm.const_int_of_string llty digits 10 in
        (ty, llty, llval)
    | Ast.ConstArr items ->
        let items = List.map (fun item -> consteval item ctx) items in
        let ity =
          match items with
          | [] -> Ast.Int (Ast.Signed 32)
          | (ty, _, _) :: tl ->
              List.iter
                (fun (ity, _, _) ->
                  if ity <> ty then error "array constant has mixed element types")
                tl;
              ty
        in
        let ty = Ast.Arr (ity, List.length items) in
        let llty = emit_ty ty ctx in
        let elems = Array.of_list (List.map (fun (_, _, llval) -> llval) items) in
        let llval = Llvm.const_array (emit_ty ity ctx) elems in
        (ty, llty, llval)
    | Ast.ConstAggr items ->
        let items = List.map (fun item -> consteval item ctx) items in
        let ty = Ast.Aggr (List.map (fun (ty, _, _) -> ty) items) in
        let llty = emit_ty ty ctx in
        let items = Array.of_list (List.map (fun (_, _, llval) -> llval) items) in
        let llval = Llvm.const_struct ctx.llctx items in
        (ty, llty, llval)
  in
  let rec addreval (expr : Ast.expr) (ctx : ctx) : Ast.ty * Llvm.lltype * Llvm.llvalue =
    match expr with
    | Ast.Id id -> (
        match List.find_map (fun vars -> Hashtbl.find_opt vars id) ctx.vars with
        | Some (ty, llty, addr) -> (ty, llty, addr)
        | None -> error "undefined variable \"%s\"" id)
    | Ast.Index (base, idx) -> (
        let bty, bllty, baddr = addreval base ctx in
        match bty with
        | Ast.Arr (ety, _) ->
            let _, _, idx = emit_expr idx ctx in
            let zero = Llvm.const_int (Llvm.i32_type ctx.llctx) 0 in
            let addr = Llvm.build_gep bllty baddr [| zero; idx |] "" ctx.llbdr in
            (ety, emit_ty ety ctx, addr)
        | _ -> error "indexing requires an array")
    | Ast.Member (base, loc) -> (
        let bty, bllty, baddr = addreval base ctx in
        match bty with
        | Ast.Aggr items ->
            let ity = List.nth items loc in
            let addr = Llvm.build_struct_gep bllty baddr loc "" ctx.llbdr in
            (ity, emit_ty ity ctx, addr)
        | _ -> error "member access requires an aggregate")
    | Ast.Deref (ptr, ty) -> (
        let pty, _, pllval = emit_expr ptr ctx in
        match pty with
        | Ast.Ptr -> (ty, emit_ty ty ctx, pllval)
        | _ -> error "dereference requires a pointer")
    | _ -> error "expression is not an lvalue"
  in
  match expr with
  | Ast.Const constexpr -> consteval constexpr ctx
  | Ast.Assn (var, expr) ->
      let ty, llty, addr = addreval var ctx in
      let ety, _, llval = emit_expr expr ctx in
      if ty <> ety then error "assignment type mismatch";
      ignore (Llvm.build_store llval addr ctx.llbdr);
      (ty, llty, llval)
  | Ast.Cast (ty, expr) ->
      let sty, sllty, sllval = emit_expr expr ctx in
      let llty = emit_ty ty ctx in
      let llval =
        match (sty, ty) with
        | Ast.Int sity, Ast.Int ity -> (
            match (sity, ity) with
            | Ast.Bool, Ast.Bool -> sllval
            | Ast.Bool, _ -> Llvm.build_zext sllval llty "" ctx.llbdr
            | _, Ast.Bool ->
                Llvm.build_icmp Llvm.Icmp.Ne sllval (Llvm.const_null sllty) "" ctx.llbdr
            | _ -> (
                let sbits = Llvm.integer_bitwidth sllty in
                let bits = Llvm.integer_bitwidth llty in
                if sbits = bits then sllval
                else if sbits > bits then Llvm.build_trunc sllval llty "" ctx.llbdr
                else
                  match sity with
                  | Ast.Signed _ -> Llvm.build_sext sllval llty "" ctx.llbdr
                  | _ -> Llvm.build_zext sllval llty "" ctx.llbdr))
        | _ -> error "cast only supports integer types"
      in
      (ty, llty, llval)
  | Ast.Unary (op, expr) -> (
      let ty, llty, llval = emit_expr expr ctx in
      (match ty with Ast.Int _ -> () | _ -> error "unary operator requires integer operands");
      match op with
      | Ast.Not -> (ty, llty, Llvm.build_not llval "" ctx.llbdr)
      | Ast.LgNot ->
          let llval = Llvm.build_icmp Llvm.Icmp.Eq llval (Llvm.const_null llty) "" ctx.llbdr in
          let ty = Ast.Int Ast.Bool in
          (ty, emit_ty ty ctx, llval))
  | Ast.Binary (lhs, ((Ast.LgAnd | Ast.LgOr) as op), rhs) ->
      let lty, llty, lval = emit_expr lhs ctx in
      (match lty with Ast.Int _ -> () | _ -> error "binary operator requires integer operands");
      let bool_ty = Ast.Int Ast.Bool in
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
      if lty <> rty then error "binary operand type mismatch";
      let rcond = Llvm.build_icmp Llvm.Icmp.Ne rval (Llvm.const_null rllty) "" ctx.llbdr in
      let rhs_end = Llvm.insertion_block ctx.llbdr in
      ignore (Llvm.build_br join_blk ctx.llbdr);

      Llvm.position_at_end join_blk ctx.llbdr;
      let result = Llvm.build_phi [ (short_val, lhs_end); (rcond, rhs_end) ] "" ctx.llbdr in
      (bool_ty, bool_llty, result)
  | Ast.Binary (lhs, op, rhs) -> (
      let lty, llty, lval = emit_expr lhs ctx in
      let rty, _, rval = emit_expr rhs ctx in
      if lty <> rty then error "binary operand type mismatch";
      let ity =
        match lty with Ast.Int ity -> ity | _ -> error "binary operator requires integer operands"
      in
      let signed = match ity with Ast.Signed _ -> true | _ -> false in
      let bool_ty = Ast.Int Ast.Bool in
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
  | Ast.Addrof expr ->
      let _, _, addr = addreval expr ctx in
      let ty = Ast.Ptr in
      (ty, emit_ty ty ctx, addr)
  | Ast.Call (name, args) -> (
      match emit_call name args ctx with
      | Some resp -> resp
      | None -> error "void function used as a value")
  | Ast.Id _ | Ast.Index _ | Ast.Member _ | Ast.Deref _ ->
      let ty, llty, addr = addreval expr ctx in
      let llval = Llvm.build_load llty addr "" ctx.llbdr in
      (ty, llty, llval)

and emit_call (name : Ast.id) (args : Ast.expr list) (ctx : ctx) :
    (Ast.ty * Llvm.lltype * Llvm.llvalue) option =
  let ptys, rty, llfty, llfn =
    match Hashtbl.find_opt ctx.funcs name with
    | Some item -> item
    | None -> error "undefined function \"%s\"" name
  in
  let llargs =
    List.map2
      (fun pty arg ->
        let aty, _, llarg = emit_expr arg ctx in
        if aty <> pty then error "argument type mismatch in call to \"%s\"" name;
        llarg)
      ptys args
    |> Array.of_list
  in
  let llcall = Llvm.build_call llfty llfn llargs "" ctx.llbdr in
  match rty with Some rty -> Some (rty, emit_ty rty ctx, llcall) | None -> None

let rec emit_stmt (stmt : Ast.stmt) (ctx : ctx) : bool =
  match stmt with
  | Ast.Expr (Ast.Call (name, args)) ->
      ignore (emit_call name args ctx);
      false
  | Ast.Expr expr ->
      ignore (emit_expr expr ctx);
      false
  | Ast.Var (id, expr) ->
      let ty, llty, llval = emit_expr expr ctx in
      let addr = Llvm.build_alloca llty id ctx.llbdr in
      ignore (Llvm.build_store llval addr ctx.llbdr);
      (match ctx.vars with
      | vars :: _ -> Hashtbl.add vars id (ty, llty, addr)
      | [] -> error "internal: unexpected empty `ctx.vars`");
      false
  | Ast.If (cond, then_stmts, else_stmts) ->
      let llfn = Llvm.block_parent (Llvm.insertion_block ctx.llbdr) in
      let then_blk = Llvm.append_block ctx.llctx "" llfn in
      let else_blk = Llvm.append_block ctx.llctx "" llfn in
      let join_blk = Llvm.append_block ctx.llctx "" llfn in

      let _, _, llcval = emit_expr cond ctx in
      ignore (Llvm.build_cond_br llcval then_blk else_blk ctx.llbdr);

      ctx.vars <- Hashtbl.create 0 :: ctx.vars;
      Llvm.position_at_end then_blk ctx.llbdr;
      let then_terminated = List.exists (fun stmt -> emit_stmt stmt ctx) then_stmts in
      if not then_terminated then ignore (Llvm.build_br join_blk ctx.llbdr);
      (match ctx.vars with
      | [] -> error "internal: unexpected empty `ctx.vars`"
      | hd :: tl -> ctx.vars <- tl);

      ctx.vars <- Hashtbl.create 0 :: ctx.vars;
      Llvm.position_at_end else_blk ctx.llbdr;
      let else_terminated = List.exists (fun stmt -> emit_stmt stmt ctx) else_stmts in
      if not else_terminated then ignore (Llvm.build_br join_blk ctx.llbdr);
      (match ctx.vars with
      | [] -> error "internal: unexpected empty `ctx.vars`"
      | hd :: tl -> ctx.vars <- tl);

      Llvm.position_at_end join_blk ctx.llbdr;
      false
  | Ast.Loop stmts ->
      let llfn = Llvm.block_parent (Llvm.insertion_block ctx.llbdr) in
      let body_blk = Llvm.append_block ctx.llctx "" llfn in
      let exit_blk = Llvm.append_block ctx.llctx "" llfn in

      ignore (Llvm.build_br body_blk ctx.llbdr);

      ctx.break_blks <- exit_blk :: ctx.break_blks;
      ctx.continue_blks <- body_blk :: ctx.continue_blks;

      Llvm.position_at_end body_blk ctx.llbdr;
      let terminated = List.exists (fun stmt -> emit_stmt stmt ctx) stmts in
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

let emit_decl (decl : Ast.decl) (ctx : ctx) : unit =
  match decl with
  | Ast.Func (id, params, rty, stmts) -> (
      (if Hashtbl.mem ctx.funcs id then error "duplicate function declaration \"%s\"" id;
       let ptys = List.map (fun (_, pty) -> pty) params in
       let llptys = Array.of_list (List.map (fun pty -> emit_ty pty ctx) ptys) in
       let llrty =
         match rty with None -> Llvm.void_type ctx.llctx | Some rty -> emit_ty rty ctx
       in
       let llfty = Llvm.function_type llrty llptys in
       let llfn = Llvm.define_function id llfty ctx.llmod in
       Hashtbl.add ctx.funcs id (ptys, rty, llfty, llfn);

       ctx.vars <- Hashtbl.create 0 :: ctx.vars;
       let entry_blk = Llvm.entry_block llfn in
       Llvm.position_at_end entry_blk ctx.llbdr;
       List.iter2
         (fun (pid, pty) llpval ->
           let llpty = emit_ty pty ctx in
           let addr = Llvm.build_alloca llpty pid ctx.llbdr in
           ignore (Llvm.build_store llpval addr ctx.llbdr);
           match ctx.vars with
           | [] -> error "internal: unexpected empty `ctx.vars`"
           | vars :: _ -> Hashtbl.add vars pid (pty, llpty, addr))
         params
         (Array.to_list (Llvm.params llfn));
       let terminated = List.exists (fun stmt -> emit_stmt stmt ctx) stmts in
       if not terminated then
         match rty with
         | None -> ignore (Llvm.build_ret_void ctx.llbdr)
         | Some _ -> error "control reaches end of non-void function \"%s\"" id);
      match ctx.vars with
      | [] -> error "internal: unexpected empty `ctx.vars`"
      | hd :: tl -> ctx.vars <- tl)
  | Ast.ExtFunc (id, ptys, rty) ->
      if Hashtbl.mem ctx.funcs id then error "duplicate function declaration \"%s\"" id;
      let llptys = Array.of_list (List.map (fun pty -> emit_ty pty ctx) ptys) in
      let llrty = match rty with None -> Llvm.void_type ctx.llctx | Some rty -> emit_ty rty ctx in
      let llfty = Llvm.function_type llrty llptys in
      let llfn = Llvm.declare_function id llfty ctx.llmod in
      Hashtbl.add ctx.funcs id (ptys, rty, llfty, llfn)

let codegen (prog : Ast.prog) (ctx : ctx) : unit = List.iter (fun decl -> emit_decl decl ctx) prog
