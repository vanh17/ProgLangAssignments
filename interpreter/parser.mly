%{
  open Types
%}
%token TRUE 
%token FALSE
%token <float> FLOAT
%token DBLSEMI
%nonassoc FLOAT

%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }
;

expr:
  | FLOAT                        { NumS $1 }
  | TRUE                         { BoolS true}
  | FALSE                        { BoolS false}
;

