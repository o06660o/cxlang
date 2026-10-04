%token EOF

%token <string> ID
%token <string> CONST_INT
%token <int> CONST_CHAR
%token <string> CONST_STRING
%token <int> INT
%token <int * bool> INT_TY

/* Keywords */
%token AUTO     /* auto */
%token BOOL     /* bool */
%token BREAK    /* break */
%token CONTINUE /* continue */
%token ELSE     /* else */
%token FALSE    /* false */
%token IF       /* if */
%token LOOP     /* loop */
%token PTR      /* ptr */
%token RETURN   /* return */
%token TRUE     /* true */
%token USING    /* using */

/* Symbols */
%token LPAREN   /* ( */
%token RPAREN   /* ) */
%token LBRACKET /* [ */
%token RBRACKET /* ] */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token COLON    /* : */
%token COMMA    /* , */
%token DOT      /* . */
%token SEMI     /* ; */
%token EQ       /* = */
%token ARROW    /* -> */
%token TILDE    /* ~ */
%token BANG     /* ! */
%token SLASH    /* / */
%token STAR     /* * */
%token PERC     /* % */
%token PLUS     /* + */
%token DASH     /* - */
%token LTLT     /* << */
%token GTGT     /* >> */
%token LT       /* < */
%token LTEQ     /* <= */
%token GT       /* > */
%token GTEQ     /* >= */
%token EQEQ     /* == */
%token BANGEQ   /* != */
%token AMP      /* & */
%token CARET    /* ^ */
%token BAR      /* | */
%token AMPAMP   /* && */
%token BARBAR   /* || */

/* Builtins */
%token CAST   /* #cast */
%token DEREF  /* #deref */
%token ADDROF /* #addrof */

%right EQ
%left BARBAR
%left AMPAMP
%left BAR
%left CARET
%left AMP
%nonassoc EQEQ BANGEQ
%nonassoc LT LTEQ GT GTEQ
%left LTLT GTGT
%left PLUS DASH
%left STAR SLASH PERC
%right TILDE BANG
%left LPAREN LBRACKET DOT

%start <Ast.prog> prog

%%

_pair_idty: id=ID COLON ty=ty { (id, ty) }
_pair_idexpr: id=ID COLON expr=expr { (id, expr) }

ty:
  | BOOL { Ast.Bool }
  | ty=INT_TY { Ast.Int ty }
  | LBRACKET ty=ty SEMI cnt=INT RBRACKET { Ast.Array (ty, cnt) }
  | LBRACE items=separated_nonempty_list(SEMI, ty) RBRACE { Ast.Tuple items }
  | LBRACE items=separated_nonempty_list(SEMI, _pair_idty) RBRACE { Ast.Struct items }
  | PTR { Ast.Ptr }
  | LBRACKET RBRACKET LPAREN items=separated_list(COMMA, ty) RPAREN rty=option(preceded(ARROW,ty))
    { Ast.Func (items, rty) }
  | id=ID { Ast.Alias id }

%inline uop:
  | TILDE { Ast.Not }
  | BANG { Ast.LgNot }

%inline bop:
  | SLASH { Ast.Div }
  | STAR { Ast.Mul }
  | PERC { Ast.Rem }
  | PLUS { Ast.Add }
  | DASH { Ast.Sub }
  | LTLT { Ast.Shl }
  | GTGT { Ast.Shr }
  | LT { Ast.Lt }
  | LTEQ { Ast.Le }
  | GT { Ast.Gt }
  | GTEQ { Ast.Ge }
  | EQEQ { Ast.Eq }
  | BANGEQ { Ast.Ne }
  | AMP { Ast.And }
  | CARET { Ast.Xor }
  | BAR { Ast.Or }
  | AMPAMP { Ast.LgAnd }
  | BARBAR { Ast.LgOr }

expr:
  | LPAREN expr=expr RPAREN { expr }
  | TRUE { Ast.NewTrue }
  | FALSE { Ast.NewFalse }
  | literal=CONST_INT { Ast.NewInt literal }
  | value=CONST_CHAR { Ast.NewChar value }
  | bytes=CONST_STRING { Ast.NewString bytes }
  | LBRACKET items=separated_nonempty_list(COMMA, expr) RBRACKET { Ast.NewArray items }
  | LBRACE items=separated_nonempty_list(COMMA, expr) RBRACE { Ast.NewTuple items }
  | LBRACE items=separated_nonempty_list(COMMA, _pair_idexpr) RBRACE { Ast.NewStruct items }
  | LBRACKET RBRACKET LPAREN items=separated_list(COMMA, _pair_idty) RPAREN
    rty=option(preceded(ARROW,ty)) body=block { Ast.NewFunc (items, rty, body) }
  | var=expr EQ expr=expr { Ast.Assn (var, expr) }
  | op=uop expr=expr { Ast.Unary (op, expr) }
  | lhs=expr op=bop rhs=expr { Ast.Binary (lhs, op, rhs) }
  | id=ID { Ast.Id id }
  | base=expr LBRACKET idx=expr RBRACKET { Ast.MemArray (base, idx) }
  | base=expr DOT loc=INT { Ast.MemTuple (base, loc) }
  | base=expr DOT loc=ID { Ast.MemStruct (base, loc) }
  | expr=expr LPAREN args=separated_list(COMMA, expr) RPAREN { Ast.Call (expr, args) }
  | CAST LPAREN ty=ty COMMA expr=expr RPAREN { Ast.Cast (ty, expr) }
  | DEREF LPAREN ptr=expr COMMA ty=ty RPAREN { Ast.Deref (ptr, ty) }
  | ADDROF LPAREN expr=expr RPAREN { Ast.Addrof expr }

stmt:
  | decl=decl SEMI { Ast.Decl decl }
  | expr=expr SEMI { Ast.Expr expr }
  | IF LPAREN cond=expr RPAREN then_stmts=block else_stmts=option(preceded(ELSE,block))
    {
      let else_stmts = match else_stmts with Some stmts -> stmts | None -> [] in
      Ast.If (cond, then_stmts, else_stmts)
    }
  | LOOP stmts=block { Ast.Loop stmts }
  | BREAK SEMI { Ast.Break }
  | CONTINUE SEMI { Ast.Continue }
  | RETURN expr=option(expr) SEMI { Ast.Return expr }

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

decl:
  | AUTO id=ID EQ expr=expr { Ast.Var (id, expr) }
  | USING id=ID EQ ty=ty { Ast.Type (id, ty) }

prog: decls=list(decl) EOF { decls }
