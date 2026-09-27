let emit_llvm = ref false

let arg_parse () : string * string =
  let infile = ref None in
  let outfile = ref "a.out" in
  let usage = "Usage: cxcc [--emit-llvm] [-o outfile] infile" in
  let options =
    [
      ("--emit-llvm", Arg.Set emit_llvm, "Output LLVM IR");
      ("-o", Arg.Set_string outfile, "Set output file path");
    ]
  in
  let set_infile (path : string) =
    match !infile with
    | None -> infile := Some path
    | Some _ -> raise (Arg.Bad "expected exactly one input file")
  in
  Arg.parse options set_infile usage;
  match !infile with
  | Some path -> (path, !outfile)
  | None ->
      Arg.usage options usage;
      exit 2

let compile (infile : string) (outfile : string) : unit =
  Llvm_all_backends.initialize ();

  let ast =
    In_channel.with_open_bin infile (fun chan ->
        let lexbuf = Lexing.from_channel chan in
        Lang.Parser.prog Lang.Lexer.token lexbuf)
  in
  let llctx = Llvm.create_context () in
  let llmod = Lang.Codegen.codegen ast llctx in

  if !emit_llvm then Llvm.print_module outfile llmod
  else
    let open Llvm_target in
    let triple = Target.default_triple () in
    let target = Target.by_triple triple in
    let machine = TargetMachine.create ~triple target in
    Llvm.set_target_triple triple llmod;
    TargetMachine.emit_to_file llmod CodeGenFileType.ObjectFile outfile machine

let () =
  let infile, outfile = arg_parse () in
  compile infile outfile
