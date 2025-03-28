%{
%}

simbolo M|D|C|L|X|V|I
numero {simbolo}+
maiuscola {simbolo}[a-z]*

%%
{numero} {printf("%s ", yytext);}
{maiuscola} {printf("%s", "");}
. {printf("%s", "");}
%%

void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

void main(){yylex();}
