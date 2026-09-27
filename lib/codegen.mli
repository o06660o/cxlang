exception Codegen_error of string

val codegen : Ast.prog -> Llvm.llcontext -> Llvm.llmodule
