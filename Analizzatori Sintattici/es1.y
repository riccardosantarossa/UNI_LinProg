%{
#include <stdio.h>
#include <stdlib.h>
void yyerror(const char *str) { fprintf(stderr,"errore: %s\n",str);} 
extern int yylex();
extern int yyparse();

typedef struct node {
  char* operation;
  int value;
  struct node* left;
  struct node* right;
} Node;

Node* nodo(char* operation, Node* left, Node* right);
Node* foglia(int value);
void outputTree(Node* radice, int spacing);
%}

%union {
  struct node* node; 
  int val;
}

%token <val> NUMBER
%token PIU MENO PER DIVISO NEWLINE
%type <node> expression

%%

input:
   | input expression NEWLINE {
       printf("Albero sintattico:\n");
       outputTree($2, 0);
   }
;

expression:
      NUMBER                       { $$ = foglia($1); }
    | expression expression PIU    { $$ = nodo("+", $1, $2); }
    | expression expression MENO   { $$ = nodo("-", $1, $2); }
    | expression expression PER    { $$ = nodo("*", $1, $2); }
    | expression expression DIVISO { $$ = nodo("/", $1, $2); }
;

%%

Node* foglia(int value) {
  Node* n = malloc(sizeof(Node));
  n->value = value;
  n->operation = NULL;
  n->left = NULL;
  n->right = NULL;
  return n;
}

Node* nodo(char* operation, Node* left, Node* right) {
  Node* n = malloc(sizeof(Node));
  n->operation = operation;
  n->left = left;
  n->right = right;
  return n;
}

void outputTree(Node* radice, int spacing) {
  if (!radice)
    return;

  for (int i = 0; i < spacing; i++)
    printf(" ");

  if (radice->operation)
    printf("%s\n", radice->operation);
  else
    printf("%d\n", radice->value);

  outputTree(radice->left, spacing + 2);
  outputTree(radice->right, spacing + 2);
}

int main() {
  return yyparse();
}