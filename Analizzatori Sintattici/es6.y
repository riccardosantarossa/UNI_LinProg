%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void yyerror(const char *str) { fprintf(stderr,"errore: %s\n",str);} 
extern int yylex();
extern int yyparse();

// Output struct
typedef struct {
    char *msg;
} Output;

Output *outputs = NULL;
int outputCount = 0;

void addOutput(const char *msg)
{
    outputs = realloc(outputs, (outputCount + 1) * sizeof(Output));
    outputs[outputCount].msg = strdup(msg);
    outputCount++;
}

void printOutput()
{
    printf("\nAlbero sintattico:\n\n");
    for (int i = outputCount - 1; i >= 0; i--)
    {
        printf("%s\n  ", outputs[i].msg);
    }
    outputs = NULL;
    outputCount = 0;
}
%}

%union {
    char *id;
    int val;
}

%token LET IN CASE APPL OF
%token <val> NUMBER
%token <id> ID

%left ID NUMBER

%%

input:
     
    | input expression '\n' { printOutput(); }
;

expression:
      simpleExpression
    | applicationExpression
    | LET equals IN expression { addOutput("LET"); }
    | CASE expression OF caseData { addOutput("CASE"); }
;

equals: 
      simpleExpression '=' simpleExpression
    | simpleExpression '=' simpleExpression equals  
;

simpleExpression:
      NUMBER { addOutput("INT"); }
    | ID     { addOutput("VAR"); }
;

applicationExpression:
      ID simpleExpression { addOutput("APP"); }
    | applicationExpression simpleExpression 
;

caseData:
      caseElement
    | caseData '|' caseElement	
;

caseElement:
      applicationExpression APPL applicationExpression
    | simpleExpression APPL applicationExpression
    | applicationExpression APPL simpleExpression 
    | simpleExpression APPL simpleExpression  
;

%%

int main(){ 
    return yyparse();
}
