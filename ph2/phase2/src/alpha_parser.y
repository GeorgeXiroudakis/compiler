%{
#include <stdio.h>
int yyerror (char* yyProvideMessage);
int yylex(void); 

extern int yylineno;
extern char* yytext;
extern FILE* yyin;


//typedef enum keywords { IF } keyword;

%}

%union{ 
	char* 	keywordVal;
	char* 	punctuationVal;
    	char*	stringVal;
    	char*	idVal;
    	char* 	operatorVal;
	int 	intVal;
	double	doubleVal;
}

%token<keywordVal> IF ELSE WHILE FOR FUNCTION RETURN BREAK CONTINUE AND NOT OR LOCAL TRUE FALSE NIL

%token<operatorVal> EQUAL PLUS MINUS ASTERISK DIVISION MODULO COMPARISON UNEQUAL PLUSPLUS MINUSMINUS GREATERTHAN LESSTHAN GREATEREQUAL LESSEQUAL

%token<punctuationVal> SQBRACKETOPEN SQBRACKETCLOSE CURBRACKETOPEN CURBRACKETCLOSE PARENTHOPEN PARENTHCLOSE SEMICOLON COMMA COLON DOT DOUBLEDOT DOUBLECOLON

%token<stringVal> STRING

%token<intVal> INTEGER

%token<doubleVal> REAL

%token<idVal> IDENTIFIER WRONGIDENT

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

