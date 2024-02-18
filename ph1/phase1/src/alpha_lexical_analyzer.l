%{
#include <stdio.h>
#include <stdlib.h>

#define YY_DECL int alpha_yylex (void* yylval)

struct alpha_token_t {
  unsigned int     numline;
  unsigned int     numToken;
  char          *content;
  char          *type;
  struct alpha_token_t *alpha_yylex;
};

%}

%option noyywrap
%option yylineno
%option header-file="./alpha_lexical_analyzer.h"

DIGIT       [0-9]
ID          [a-zA-Z][a-zA-Z0-9]*

%%
{DIGIT}+    {
	
	printf("Number at line:%d %s\n", yylineno, yytext); 
	printf("%d\n", ((struct alpha_token_t*)yylval)->numline);
	((struct alpha_token_t*)yylval)->numline++;
	}

{ID}        { printf("Identifier at line:%d %s\n", yylineno, yytext); 
				
				printf("%d\n", ((struct alpha_token_t*)yylval)->numline);
			}
.           { printf("Unexpected character: %s\n", yytext); }

%%

int main() {
    struct alpha_token_t *yylval = malloc(sizeof(struct alpha_token_t));
	yylval->numline = 1;
    alpha_yylex (yylval);
	
	printf("ff\n");
    return 0;
}

