%{

#include<string.h>
#include <stdlib.h>

int numConversion(char *num, int b);
char* trimString(char *str, char *arr);

%}

segno \+|-
decimale {segno}*[0-9]+
esadecimale 0x[0-9a-fA-F]+|[0-9a-fA-F]+[H]
ottale 0o[0-7]+|[0-7]+[O]

%%

{segno}*{decimale} {printf("decimale, %s", yytext);}
{esadecimale} {
                char *arr = malloc(yyleng * sizeof(char));
                arr = trimString(yytext, arr);
                printf("esadecimale %d", numConversion(arr, 16));
                free(arr);
              }
{ottale} { 
           char *arr = malloc(yyleng * sizeof(char));
           arr = trimString(yytext, arr);
           printf("ottale %d", numConversion(arr, 8));
           free(arr);
         }

%%

void yyerror(const char *str){fprintf(stderr, "errore: %s\n", str);}

int yywrap(){return 1;}

char *  trimString(char *str, char *arr)
{  
   //Tolgo i primi due caratteri se la stringa è del tipo "0x" / "0o"
   if(str[1] == 'x' ||  str[1] == 'o')
   {
     strncpy(arr, str + 2, strlen(str) - 2);
     //Terminatore nullo
     arr[strlen(str) - 2] = '\0';
   }
   
   //Tolgo l'ultimo carattere se la stringa è del tipo "H" / "O"
   if(str[strlen(str) - 1] == 'H' || str[strlen(str) - 1] == 'O')
   {
     strncpy(arr, str, strlen(str) - 1); 
     //Terminatore nullo
     arr[strlen(str) - 1] = '\0'; 
   }

   return arr;
}


int numConversion(char *num, int b)
{
  
  int val = 0;

  for(int i = 0; i < strlen(num); i++)
  {
	  
    val = val * b;
    switch(num[i])
    {
          case '1':
		val += 1;
	  break;

	  case '2':
		val += 2;
	  break;
	  
	  case '3':
		val += 3;
	  break;
	  
	  case '4':
		val += 4;
	  break;
	  
	  case '5':
		val += 5;
	  break;
	  
	  case '6':
		val += 6;
	  break;
	  
	  case '7':
		val += 7;
	  break;
	  
	  case '8':
		val += 8;
	  break;
	  
	  case '9':
		val += 9;
	  break;
	  
	  case '0':
	  break;

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
    }
  }
   return val;
}

void main(){yylex();};
