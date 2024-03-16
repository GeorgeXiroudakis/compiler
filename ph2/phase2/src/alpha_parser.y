%{
#include <stdio.h>
#include <stdlib.h>

#define RESET   "\033[0m"
#define RED     "\033[31m"

int yyerror (char* yyProvideMessage);
int yylex(void); 

extern int yylineno;
extern char* yytext;
extern FILE* yyin;


%}

%start program

%union{ 
	char* 	keywordVal;
	char* 	punctuationVal;
    	char*	stringVal;
    	char*	idVal;
    	char* 	operatorVal;
	int 	intVal;
	double	realVal;
}



%token<keywordVal> IF ELSE WHILE FOR FUNCTION RETURN BREAK CONTINUE AND NOT OR LOCAL TRUE FALSE NIL

%token<operatorVal> EQUAL PLUS MINUS ASTERISK DIVISION MODULO COMPARISON UNEQUAL PLUSPLUS MINUSMINUS GREATERTHAN LESSTHAN GREATEREQUAL LESSEQUAL

%token<punctuationVal> SQBRACKETOPEN SQBRACKETCLOSE CURBRACKETOPEN CURBRACKETCLOSE PARENTHOPEN PARENTHCLOSE SEMICOLON COMMA COLON DOT DOUBLEDOT DOUBLECOLON

%token<stringVal> STRING

%token<intVal> INTEGER

%token<doubleVal> REAL

%token<idVal> IDENTIFIER WRONGIDENT



%right		EQUAL
%left		OR
%left		AND
%nonassoc	COMPARISON UNEQUAL
%nonassoc	GREATERTHAN LESSTHAN GREATEREQUAL LESSEQUAL


%left 		PLUS

%left		ASTERISK DIVISION MODULO
%right		NOT PLUSPLUS MINUSMINUS MINUS
%left 		DOT DOUBLEDOT
%left		SQBRACKETOPEN SQBRACKETCLOSE
%left		PARENTHOPEN PARENTHCLOSE

%%

program: stmt
       | program stmt
       ;

stmt: expr SEMICOLON
    | ifstmt 
    | whilestmt
    | forstmt
    | returnstmt
    | BREAK SEMICOLON
    | CONTINUE SEMICOLON
    | block
    | funcdef
    | SEMICOLON
    ;

expr: assignexpr
    | expr op expr %prec PLUS
    | term
    ;

op: PLUS 
  | MINUS
  | ASTERISK
  | DIVISION
  | MODULO
  | GREATERTHAN
  | GREATEREQUAL
  | LESSTHAN
  | LESSEQUAL
  | COMPARISON
  | UNEQUAL
  | AND
  | OR
  ;

term: PARENTHOPEN expr PARENTHCLOSE
    | MINUS expr
    | NOT expr
    | PLUSPLUS lvalue
    | lvalue PLUSPLUS
    | MINUSMINUS lvalue
    | lvalue MINUSMINUS
    | primary
    ;

assignexpr: lvalue EQUAL expr
	  ;

primary: lvalue
       | call
       | objectdef
       | PARENTHOPEN funcdef PARENTHCLOSE
       | const
       ;

lvalue: IDENTIFIER
      | LOCAL IDENTIFIER
      | DOUBLECOLON IDENTIFIER
      | member
      ;

member: lvalue DOT IDENTIFIER
      | lvalue SQBRACKETOPEN expr SQBRACKETCLOSE
      | call DOT IDENTIFIER
      | call SQBRACKETOPEN expr SQBRACKETCLOSE
      ;

call: call PARENTHOPEN elist PARENTHCLOSE
    | lvalue callsuffix
    | PARENTHOPEN funcdef PARENTHCLOSE PARENTHOPEN elist PARENTHCLOSE
    ;

callsuffix: normcall
	  | methodcall
	  ;

normcall: PARENTHOPEN elist PARENTHCLOSE
	;

methodcall: DOUBLEDOT IDENTIFIER PARENTHOPEN elist PARENTHCLOSE
	  ;

elist: expr 
     | elist COMMA expr
     ;

indexed: indexedelem
        | indexed COMMA indexedelem
        ;

objectdef: SQBRACKETOPEN  SQBRACKETCLOSE
	 | SQBRACKETOPEN elist SQBRACKETCLOSE
	 | SQBRACKETOPEN indexed SQBRACKETCLOSE
	 ;

indexedelem: CURBRACKETOPEN expr COLON expr CURBRACKETCLOSE
	   ;

block: CURBRACKETOPEN stmt_list CURBRACKETCLOSE
     ;

stmt_list: stmt
	 | stmt stmt_list
	 |
	 ;

funcdef: FUNCTION IDENTIFIER PARENTHOPEN idlist PARENTHCLOSE block
       | FUNCTION PARENTHOPEN idlist PARENTHCLOSE block
       ;

const: number
     | STRING
     | NIL
     | TRUE
     | FALSE
     ;

number: INTEGER
      | REAL
      ;

idlist: IDENTIFIER
      | idlist COMMA IDENTIFIER
      |
      ;

ifstmt: IF PARENTHOPEN expr PARENTHCLOSE
      | IF PARENTHOPEN expr PARENTHCLOSE ELSE stmt
      ;

whilestmt: WHILE PARENTHOPEN expr PARENTHCLOSE stmt
	 ;

forstmt: FOR PARENTHOPEN elist SEMICOLON expr SEMICOLON elist PARENTHCLOSE stmt
       ;

returnstmt: RETURN SEMICOLON
	  | RETURN expr SEMICOLON
	  ;

%%


int yyerror(char *yyProvideMessage) {
	fprintf(stderr, RED "%s: at line %d, before token: %s\n\n" RESET, yyProvideMessage, yylineno, yytext);
    fprintf(stderr, RED "INPUT NOT VALID\n" RESET);
    return 0;
}




int main(int argc, char **argv) {
	FILE *inputFile;

	if(argc > 2){
		fprintf(stderr, RED "Wrong call of alpha_parser\ncall with one optional command line argument (the file to analyze)\n" RESET);
		exit(EXIT_FAILURE);
	}

	if(argc == 2){
		inputFile = fopen(argv[1], "r");
		if(inputFile == NULL){
			fprintf(stderr, RED "can not open input file\n" RESET);
			exit(EXIT_FAILURE);
		}
		yyin = inputFile;
	}else
		yyin = stdin;
   

    yyparse();

    return 0;
}

