%{
%}

%%
var|function|procedure|while|do|if|else|for  {printf("parola chiave, %s", yytext);}
[a-z][a-zA-z0-9_-]* {printf("identificatore, %s", yytext);}
[0-9]+ {printf("costante numerica, %s", yytext);}
\+|\+\+|-|--|=|== {printf("operatore, %s", yytext);}
%%

void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

void main(){yylex();} 

