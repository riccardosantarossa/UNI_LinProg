%{
#include<string.h>
#include <stdlib.h>

int numConversion(char *num, int b);
char * trimString(char *str);
arr = (char*) calloc(100, sizeof(char));
%}

segno \+|-
decimale {segno}*[0-9]+
esadecimale 0x[0-9a-fA-F]+|[0-9a-fA-F]+[H]
ottale 0o[0-7]+|[0-7]+[O]

%%
{segno}*{decimale} {printf("decimale, %s", yytext);}
{esadecimale} {printf("esadecimale %d", numConversion(trimString(yytext), 16));}
{ottale} {printf("ottale %d", numConversion(trimString(yytext), 8));}
%%

void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

void trimString(char *str)
{
  
   //Tolgo i primi due caratteri se la stringa è del tipo "0x" / "0o"
   if((str[0] == '0' && str[1] == 'x') || (str[0] == '0' && str[1] == 'o'))
     strncpy(arr, str + 2, strlen(str) - 2);
   
   //Tolgo l'ultimo carattere se la stringa è del tipo "H" / "O"
   if(str[strlen(str) - 1] == 'H' || str[strlen(str)] == 'O')
     strncpy(arr, str, strlen(str) - 1);
   
}


int numConversion(char * num, int b)
{
  int val = 0, d = 0;

  for(int i = 0; i < strlen(num); i++)
  {
    val *= b;
    switch(num[i])
    {
      //Codifica dei caratteri esadecimali letterali
      case 'a':
      case 'A':
        val = val + 10;
      break;

      case 'b':
      case 'B':
        val = val + 11;
      break;

      case 'c':
      case 'C':
        val = val + 12;
      break;

      case 'd':
      case 'D':
        val = val + 13;
      break;

      case 'e':
      case 'E':
        val = val + 14;
      break;

      case 'f':
      case 'F':
        val = val + 15;
      break;

      default:
      {
        d = num[i];
        val = val + d;
      }
        
    }
  }
   return val;
}

void main(){yylex();}
