%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"
%}

%%

[ \t]+              ;
\n                  {return '\n';}
"let"               { return LET; }
"in"                { return IN; }
"case"              { return CASE;}
"of"                { return OF; }
"->"                { return APPL; }
[+-]?[0-9]+         { yylval.val = atoi(yytext); return NUMBER; }
"="                 { return '='; }
[a-z][a-zA-Z0-9_]*  { yylval.id = strdup(yytext); return ID; }
"|"                 { return '|'; }
.                   { }
%%

int yywrap() {
    return 1;
}
