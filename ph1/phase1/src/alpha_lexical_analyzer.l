%{
#include <stdio.h>
#include <stdlib.h>

#define YY_DECL int alpha_yylex (void* yylval)

typedef struct{
  unsigned int     numline;
  unsigned int     numToken;
  char	           *content;
  char             *type;
  struct           alpha_token_t *alpha_yylex;
}alpha_token_t;

%}

%option noyywrap
%option yylineno
%option header-file="./alpha_lexical_analyzer.h"

IF				if
ELSE			else
WHILE			while
FOR				for
FUNCTION		function
RETURN			return
BREAK			break
CONTINUE		continue
AND				and
NOT				not
OR				or
LOCAL			local
TRUE			true
FALSE			false
NIL				nil

EQUAL			=
PLUS			\+
MINUS			-
ASTERISK		\*
DIVISION		\/
MODULO			%
COMPARISON		==
UNEQUAL			!=
PLUSPLUS		\+\+
MINUSMINUS		--
GREATERTHAN		>
LESSTHAN		<
GREATEREQUAL	>=
LESSEQUAL		=<

DIGIT			[0-9]+

REALDIGIT		{DIGIT}\.{DIGIT}


STRING			\"(\\.|[^\\"])*\"

SQBRACKETOPEN	\[
SQBRACKETCLOSE	\]
CURBRACKETOPEN	\{
CURBRACKETCLOSE \}
PARENTHOPEN		\(
PARENTHCLOSE	\)
SEMICOLON		;
COMMA			,
COLON			:
DOUBLECOLON		::
DOT				\.
DOUBLEDOT		\.\.		

IDENTIFIER		[A-Za-z][A-Za-z_0-9]*

SINGLECOM		\/\/
MULTICOM		\/\*

WHITESPACE		[ \t\n]+

WRONGIDENT		{DIGIT}(_|{IDENTIFIER})+


%%

{IF}				{ printf("IF: %s\n", yytext); }


{ELSE}				{ printf("ELSE: %s\n", yytext); }


{WHILE}				{ printf("WHILE: %s\n", yytext); }


{FOR}				{ printf("FOR: %s\n", yytext); }


{FUNCTION}			{ printf("FUNCTION: %s\n", yytext); }


{RETURN}			{ printf("RETURN: %s\n", yytext); }


{BREAK}				{ printf("BREAK: %s\n", yytext); }


{CONTINUE}			{ printf("CONTINUE: %s\n", yytext); }


{AND}				{ printf("AND: %s\n", yytext); }


{NOT}				{ printf("NOT: %s\n", yytext); }


{OR}				{ printf("OR: %s\n", yytext); }


{LOCAL}				{ printf("LOCAL: %s\n", yytext); }


{TRUE}				{ printf("TRUE: %s\n", yytext); }


{FALSE}				{ printf("FALSE: %s\n", yytext); }


{NIL}				{ printf("NIL: %s\n", yytext); }


{EQUAL}             { printf("EQUAL: %s\n", yytext); }


{PLUS}              { printf("PLUS: %s\n", yytext); }


{MINUS}             { printf("MINUS: %s\n", yytext); }


{ASTERISK}          { printf("ASTERISK: %s\n", yytext); }


{DIVISION}          { printf("DIVISION: %s\n", yytext); }


{MODULO}            { printf("MODULO: %s\n", yytext); }


{COMPARISON}        { printf("COMPARISON: %s\n", yytext); }


{UNEQUAL}           { printf("UNEQUAL: %s\n", yytext); }


{PLUSPLUS}          { printf("PLUSPLUS: %s\n", yytext); }


{MINUSMINUS}        { printf("MINUSMINUS: %s\n", yytext); }


{GREATERTHAN}       { printf("GREATERTHAN: %s\n", yytext); }


{LESSTHAN}          { printf("LESSTHAN: %s\n", yytext); }


{GREATEREQUAL}      { printf("GREATEREQUAL: %s\n", yytext); }


{LESSEQUAL}         { printf("LESSEQUAL: %s\n", yytext); }


{DIGIT}				{ printf("DIGIT: %s\n", yytext); }


{REALDIGIT}			{ printf("REALDIGIT: %s\n", yytext); }


{STRING}			{ printf("STRING: %s\n", yytext); }


{SQBRACKETOPEN}     { printf("SQBRACKETOPEN: %s\n", yytext); }


{SQBRACKETCLOSE}    { printf("SQBRACKETCLOSE: %s\n", yytext); }


{CURBRACKETOPEN}    { printf("CURBRACKETOPEN: %s\n", yytext); }


{CURBRACKETCLOSE}   { printf("CURBRACKETCLOSE: %s\n", yytext); }


{PARENTHOPEN}		{ printf("PARENTHOPEN: %s\n", yytext); }


{PARENTHCLOSE}      { printf("PARENTHCLOSE: %s\n", yytext); }


{SEMICOLON}         { printf("SEMICOLON: %s\n", yytext); }


{COMMA}             { printf("COMMA: %s\n", yytext); }


{COLON}             { printf("COLON: %s\n", yytext); }


{DOUBLECOLON}       { printf("DOUBLECOLON: %s\n", yytext); }


{DOT}				{ printf("DOT: %s\n", yytext); }


{DOUBLEDOT}         { printf("DOUBLEDOT: %s\n", yytext); }


{IDENTIFIER}		{ printf("IDENTIFIER: %s\n", yytext); }


{SINGLECOM}         {
						int c;
						do{
							c = input();
						}while(c != '\n' || c != EOF);

						return 0;
					}


{MULTICOM}			{
						printf("MULTICOM: %s\n", yytext); 

						int c;
						while ((c = input()) != EOF){
							if(c == '*'){
								c = input();
								if(c == '/')break;
								else unput(c);
							}
						}

						return 0;
					}


{WHITESPACE}		;


{WRONGIDENT}		{ printf("Wrong ID: %s\n", yytext); }


.					{ printf("Unexpected character: %s\n", yytext); }
%%

/*void printLexAnalysis()*/

int main() {
    alpha_token_t *yylval;
    alpha_yylex (yylval);
	
	printf("doing print\n");
    return 0;
}

