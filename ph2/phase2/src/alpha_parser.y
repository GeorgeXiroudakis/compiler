%{
#include <iostream.h>
int yyerror (char* yyProvideMessage);
int alpha_yylex(void);

extern int yylineno;
extern char* yytext;
extern FILE* yyin;
extern keyword;

%}
%



%start program

%union {keyword keywordVal}

%token<keywordVal> IF

#................to be continiued







#priorities!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


%%
program: IF
	   |
	   ;

IF: "if" {std::cout << $1;}

#grammar

%%

int main(int argc, char **argv) {
	yyparse();
	return 0;
}
