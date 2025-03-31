%{
#include<string.h>
int numConversion(char *num, int b);
%}

segno \+|-
decimale {segno}*[0-9]+
esadecimale 0x[0-9a-fA-F]+|[0-9a-fA-F]+[H]
ottale 0o[0-7]+|[0-7]+[O]

%%
{segno}*{decimale} {printf("decimale, %s", yytext);}
{esadecimale} {printf("esadecimale, %d", numConversion(yytext, 16));}
%%

void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

int numConversion(char * num, int b)
{
  int val = 0;

  if(!strcmp(num[0], '0') && !stcrmp(num[1], 'x') || !strcmp(num[0], '0') && !stcrmp(num[1], 'o'))
    num = num[2:];
 
  if(!strcmp(num[strlen(num) - 1], 'H') || !strcmp(num[strlen(num)-1], 'O'))
    num = num[0:strlen(num) - 1];

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
        int d = num[i];
        val = val + d;
      }
        
    }
  }
   return val;
}

void main(){yylex();} 
