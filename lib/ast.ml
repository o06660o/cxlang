type id = string

type ty = Int of ity | Arr of ty * int | Aggr of ty list | Ptr
and ity = Bool | Signed of int | Unsigned of int

type const = True | False | ConstInt of string | ConstArr of const list | ConstAggr of const list

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
  | Const of const
  | Id of id
  | Assn of expr * expr
  | Cast of ity * expr
  | Unary of uop * expr
  | Binary of expr * bop * expr
  | Index of expr * expr
  | Member of expr * int
  | Deref of expr * ty
  | Addrof of expr
  | Call of id * expr list

type stmt =
  | Expr of expr
  | Var of id * expr
  | If of expr * stmt list * stmt list
  | Loop of stmt list
  | Break
  | Continue
  | Return of expr option

type decl =
  | Func of id * (id * ty) list * ty option * stmt list
  | ExtFunc of id * ty list * ty option

type prog = decl list
