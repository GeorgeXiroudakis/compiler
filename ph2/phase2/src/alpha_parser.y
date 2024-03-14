%{
#include <stdio.h>
int yyerror (char* yyProvideMessage);
int yylex(void); 

extern int yylineno;
extern char* yytext;
extern FILE* yyin;


//typedef enum keywords { IF, ELSE } keyword;

%}

%union {
    int keywordVal; 
}

%token<keywordVal> IF

%%

program: IF
       |
       ;

%%


int yyerror(char *s) {
    fprintf(stderr, "Error: %s\n", s);
    return 0;
}




int main(int argc, char **argv) {
    
    
    yyin = stdin;
    yyparse();
    return 0;
}

