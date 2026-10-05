type id = string

type ty =
  | Bool
  | Int of (int * bool)
  | Array of ty * int
  | Ptr
  | Func of ty list * ty option
  | Named of id

type uop = (* ~ *) Not | (* ! *) LgNot

and bop =
  | (* / *) Div
  | (* * *) Mul
  | (* % *) Rem
  (* ------------- *)
  | (* + *) Add
  | (* - *) Sub
  (* ------------- *)
  | (* << *) Shl
  | (* >> *) Shr
  (* ------------- *)
  | (* < *) Lt
  | (* <= *) Le
  | (* > *) Gt
  | (* >= *) Ge
  (* ------------- *)
  | (* == *) Eq
  | (* != *) Ne
  (* ------------- *)
  | (* & *) And
  (* ------------- *)
  | (* ^ *) Xor
  (* ------------- *)
  | (* | *) Or
  (* ------------- *)
  | (* && *) LgAnd
  (* ------------- *)
  | (* || *) LgOr

type expr =
  | NewTrue
  | NewFalse
  | NewInt of string
  | NewChar of int
  | NewString of string
  | NewArray of expr list
  | NewStruct of id * (id * expr) list
  | NewFunc of (id * ty) list * ty option * block
  | Assn of expr * expr
  | Unary of uop * expr
  | Binary of expr * bop * expr
  | Id of id
  | MemArray of expr * expr
  | MemStruct of expr * id
  | Call of expr * expr list
  | Cast of ty * expr
  | Deref of expr * ty
  | Addrof of expr

and stmt =
  | Var of id * expr
  | Expr of expr
  | If of expr * block * block
  | Loop of block
  | Break
  | Continue
  | Return of expr option

and block = stmt list

type gdecl = Global of id * expr | Struct of id * (id * ty) list
type prog = gdecl list
