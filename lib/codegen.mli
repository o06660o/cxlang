type ctx

val ctx_create : Llvm.llcontext -> ctx
val codegen : Ast.prog -> ctx -> unit
