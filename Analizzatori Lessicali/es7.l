%{
%}

segno \+|-
numerointero {segno}*[0-9]+
frazione [0-9]+[.][0-9]*
floatingIntero E{numerointero}
floatingFrazione {numerointero}e|{frazione}e

%%
{segno}{numerointero} {printf("%s, numero intero, %d", yytext, yyleng - 1);}
{numerointero} {printf("%s, numero intero, %d", yytext, yyleng);}
{segno}{frazione} {printf("%s, numero frazionario, %d", yytext, yyleng - 2);}
{frazione} {printf("%s, numero frazionario, %d", yytext, yyleng - 1);}
{floatingIntero} {printf("%s, floating point, %d", yytext,  yyleng - 1 );}
{floatingFrazione} {printf("%s, floating point, %d", yytext, yyleng - 2 );}
%%

void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

void main(){yylex();}