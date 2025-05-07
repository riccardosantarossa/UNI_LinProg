%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char* InfToPart(char *input, char *buf, int con);
char* PartToInf(char *input, char *buf);

%}

radice [a-zA-z]+
primaCon {radice}are
secondaCon{radice}ere
terzaCon{radice}ire
verboParticipio {radice}ato|{radice}ito|{radice}uto

%%
primaCon  {
            char *buf = malloc(yyleng*sizeof(char));
            printf("%s\n", InfToPart(yytext, buf, 1));
            free(buf);
          }

secondaCon  {
              char *buf = malloc(yyleng*sizeof(char));
              printf("%s\n", InfToPart(yytext, buf, 2));
              free(buf);
            }

terzaCon  {
            char *buf = malloc(yyleng*sizeof(char));
            printf("%s\n", InfToPart(yytext, buf, 3));
            free(buf);
          }

verboParticipio {
                  char *buf = malloc(yyleng * sizeof(char));
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
  buf[strlen(buf) - 1] = '\0';

  if(con == 1)
    strcat(buf, suff);
  else
    strcat(buf, suff2);
  
  return buf;
}

char* PartToInf(char *input, char *buf)
{
   //Salvo la desinenza del participio
   char *des = malloc(4 * sizeof(char));
   strncpy(des, input + (strlen(input) - 3), 3);
   des[3] = '\0';
   
   //Rimuovo le ultime la desinenza dall'input e aggiungo la 
   //coniugazione corretta 
   strncpy(buf, input, strlen(input) - 3);
   buf[strlen(buf) - 1] = '\0';

   if(strcmp(des, "ato") == 0)
    strcat(buf, "are\0");
   else if(strcmp(des, "uto") == 0)
    strcat(buf, "ere\0");
   else
    strcat(buf, "ire\0");

   free(des);

   return buf; 
}

void main(){yylex();}
