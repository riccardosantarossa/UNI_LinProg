%{
#include <stdio.h>
#include <stdlib.h> 
#include "y.tab.h"
%}

%%
[+-]?[0-9]+ {yylval.val = atoi(yytext); return NUMBER; /*riconosce numero con segno*/}
"+" {return PIU;}
"-" {return MENO;}
"*" {return PER;}
"/" {return DIVISO;}
\n {return NEWLINE;}
[ \t]+ {} 
. {}
%%

int yywrap() {
    return 1;
}
