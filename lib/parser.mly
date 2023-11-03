%{
  open Types 
%}

%token S
%token U
%token SEP
%token <int> NUM
%token EOF

%start <command> command

%%

command:
  | S; i = NUM; SEP; j = NUM; EOF { S (i,j) }
  | U; i = NUM; SEP; j = NUM; EOF { U (i,j) }
;