%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YY_DECL int alpha_yylex (void* yylval)

unsigned int tokencounter = 0;

typedef struct alpha_token_t{
  unsigned int     numline;
  unsigned int     numToken;
  char	           *content;
  char             *type;
  struct           alpha_token_t *next;
}alpha_token_t;

alpha_token_t *insertAlphaToken(alpha_token_t *head, alpha_token_t *newNode);

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

{IF}				{ printf("IF: %s\n", yytext);

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						yylval =  insertAlphaToken(yylval, newNode);
					}


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
						printf("SINGELECOM: %s\n", yytext); 

						int c;
						do{
							c = input();
						}while(c != '\n' && c != EOF);

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

					}


{WHITESPACE}		;


{WRONGIDENT}		{ printf("Wrong ID: %s\n", yytext); }


.					{ printf("Unexpected character: %s\n", yytext); }
%%


alpha_token_t *insertAlphaToken(alpha_token_t *head, alpha_token_t *newNode){
	
	if(newNode == NULL) {
		perror("newNode == NULL\n");
		return NULL;
	}	
	
	if(head == NULL){
		head = newNode;
		return head;
	}
	
	alpha_token_t *q = head;
	
	while(q->next != NULL){
		q = q->next;	
	}
	
	q->next = newNode;
	return head;
}


void printTokenList(alpha_token_t *head){
	alpha_token_t *q = head;

	while(q != NULL){
		printf("%u: #%u %s %s\n", q->numline, q->numToken, q->content, q->type);

		q = q->next;
	}
}



int main() {
    alpha_token_t *yylval = malloc(sizeof(alpha_token_t));
    alpha_yylex (yylval);
		/*printf("%u: #%u %s %s\n", yylval->numline, yylval->numToken, yylval->content, yylval->type);*/
	printTokenList(yylval);
    return 0;
}

