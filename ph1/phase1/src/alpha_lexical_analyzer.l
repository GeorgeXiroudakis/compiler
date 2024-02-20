%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YY_DECL int alpha_yylex (void* yylval)

unsigned int tokencounter = 1;

typedef struct alpha_token_t{
  unsigned int     numline;
  unsigned int     numToken;
  char	           *content;
  char             *type;
  struct           alpha_token_t *next;
}alpha_token_t;

void insertAlphaToken(alpha_token_t **head, alpha_token_t *newNode);

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
LESSEQUAL		<=

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
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/
					}


{ELSE}				{ printf("ELSE: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/
					
					}


{WHILE}				{ printf("WHILE: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{FOR}				{ printf("FOR: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{FUNCTION}			{ printf("FUNCTION: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{RETURN}			{ printf("RETURN: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{BREAK}				{ printf("BREAK: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{CONTINUE}			{ printf("CONTINUE: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{AND}				{ printf("AND: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{NOT}				{ printf("NOT: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{OR}				{ printf("OR: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{LOCAL}				{ printf("LOCAL: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{TRUE}				{ printf("TRUE: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{FALSE}				{ printf("FALSE: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{NIL}				{ printf("NIL: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "KEYWORD";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{EQUAL}             { printf("EQUAL: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{PLUS}              { printf("PLUS: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{MINUS}             { printf("MINUS: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{ASTERISK}          { printf("ASTERISK: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{DIVISION}          { printf("DIVISION: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{MODULO}            { printf("MODULO: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{COMPARISON}        { printf("COMPARISON: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{UNEQUAL}           { printf("UNEQUAL: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{PLUSPLUS}          { printf("PLUSPLUS: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{MINUSMINUS}        { printf("MINUSMINUS: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{GREATERTHAN}       { printf("GREATERTHAN: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{LESSTHAN}          { printf("LESSTHAN: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{GREATEREQUAL}      { printf("GREATEREQUAL: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{LESSEQUAL}         { printf("LESSEQUAL: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "OPERATOR";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


{DIGIT}				{ printf("DIGIT: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "INTCONST";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

						/*printf("%p\n", yylval);*/

					}


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


{WHITESPACE}		{ printf("WHITESPACE\n"); }


{WRONGIDENT}		{ printf("Wrong ID: %s\n", yytext); }


.					{ printf("Unexpected character: %s\n", yytext); }
%%


void insertAlphaToken(alpha_token_t **head, alpha_token_t *newNode){
	
	if(newNode == NULL) {
		perror("newNode == NULL\n");
	}	
	
	if(*head == NULL){
		*head = newNode;
		return;
	}
	
	alpha_token_t *q = *head;
	
	while(q->next != NULL){
		q = q->next;	
	}
	
	q->next = newNode;

}


void printTokenList(alpha_token_t **head){
	alpha_token_t *q = *head;

	while(q != NULL){
		printf("%u: #%u %s %s\n", q->numline, q->numToken, q->content, q->type);

		q = q->next;
	}
}



int main() {
    alpha_token_t *yylval = NULL;
	alpha_yylex (&yylval);
	/*printf("%p\n", yylval);*/
		/*printf("%u: #%u %s %s\n", yylval->numline, yylval->numToken, yylval->content, yylval->type);*/
	printTokenList(&yylval);
    return 0;
}

