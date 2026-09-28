type id = string

type ty = Scalar of sty | Array of ty * int | Tuple of ty list | Struct of (id * ty) list | Ptr
and sty = Bool | Int of (int * bool)

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
  | NewArray of expr list
  | NewTuple of expr list
  | NewStruct of (id * expr) list
  | Id of id
  | Assn of expr * expr
  | Cast of sty * expr
  | Unary of uop * expr
  | Binary of expr * bop * expr
  | MemArray of expr * expr
  | MemTuple of expr * int
  | MemStruct of expr * id
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
