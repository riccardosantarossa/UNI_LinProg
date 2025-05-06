%{
#include <stdlib.h>
#include <string.h>

char* InfToPart(char* input, char* buf);
char* PartToInf(char *input, char *buf);

%}

radice [a-zA-z]+
verboInfinito {radice}are|{radice}ere|{radice}ire
verboParticipio {radice}ato|{radice}ito|{radice}uto

%%
verboInfinito {
                char *buf = malloc(yyleng*sizeof(char));
                buf[strlen(buf)] = '\0';
                printf("%s\n", InfToPart(yytext, buf));
              }
%%


void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

char* InfToPart(char *input, char *buf)
{
  //Memorizza il suffisso da aggiungere
  char *suff = {"ante\0"};

  //Sostituisce la coniugazione all'infinito con quella al participio presete
  strncpy(buf, input, strlen(input) - 3);
  buf[strlen(buf)] = '\0';
  strcat(buf, suff);
  
  return buf;
}

char* PartToInf(char *input, char *buf)
{
   //Salvo la desinenza del participio
   char *des = malloc(3 * sizeof(char));
   strncpy(des, input - 4, 3);
   //des[strlen(des) - 1] = '\0';
   
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

   return buf; 
}

void main(){yylex();};

