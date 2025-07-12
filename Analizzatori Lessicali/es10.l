%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char* InfToPart(char *input, char *buf, int con);
char* PartToInf(char *input, char *buf);

%}

%%

accendere {printf("%s\n", "acceso");}

chiedere {printf("%s\n", "chiesto");}

coprire {printf("%s\n", "coperto");}

offeso {printf("%s\n", "offendere");}

fritto {printf("%s\n", "friggere");}

[a-zA-Z]+are  {
                char *buf = malloc(yyleng*sizeof(char) + 5);
                printf("%s\n", InfToPart(yytext, buf, 1));
                free(buf);
              }

[a-zA-Z]+ere  {
                char *buf = malloc(yyleng*sizeof(char) + 5);
                printf("%s\n", InfToPart(yytext, buf, 2));
                free(buf);
              }

[a-zA-Z]+ire  {
                char *buf = malloc(yyleng*sizeof(char) + 5);
                printf("%s\n", InfToPart(yytext, buf, 3));
                free(buf);
              }

[a-zA-Z]+(ito|ato|uto) {
                         char *buf = malloc(yyleng * sizeof(char) + 5);
                         printf("%s\n", PartToInf(yytext, buf));
                         free(buf);
                       }
%%

void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

char* InfToPart(char *input, char *buf, int con)
{
  //Memorizza il suffisso da aggiungere
  char *suff = {"ante\0"};
  char *suff2 = {"ente\0"};

  //Sostituisce la coniugazione all'infinito con quella al participio presente
  strncpy(buf, input, strlen(input) - 3);
  buf[strlen(input)] = '\0';

  if(con == 1)
    strcat(buf, suff);
  else
    strcat(buf, suff2);
  
  return buf;
}

char* PartToInf(char *input, char *buf)
{
   //Elimino la coniugazione del participio
   strncpy(buf, input, strlen(input) - 3);
   buf[strlen(input) - 3] = '\0';

   //Sostituisco con l'infinito corretto
   if(strstr(input, "ato"))
     strcat(buf, "are");
   else if(strstr(input, "uto"))
     strcat(buf, "ere");
   else
     strcat(buf, "ire");

   return buf; 
}

void main(){yylex();}
