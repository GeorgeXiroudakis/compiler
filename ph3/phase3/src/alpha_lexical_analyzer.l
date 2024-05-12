%{
#include "alpha_parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>

#define YY_DECL int alpha_yylex(void)

#define RESET   "\033[0m"
#define RED     "\033[31m"

unsigned int tokencounter = 1;

%}

%option noyywrap
%option yylineno
%option header-file="src/alpha_lexical_analyzer.h"

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

INTEGER			[0-9]+

REAL		{INTEGER}\.{INTEGER}


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

WRONGIDENT		{INTEGER}|_(_|{IDENTIFIER})+


%%

{IF}				{
						yylval.keywordVal = strdup(yytext);
						return IF;
					}

{ELSE}				{	
						yylval.keywordVal = strdup(yytext);
						return ELSE;
					}

{WHILE}				{
						yylval.keywordVal = strdup(yytext);
						return 	WHILE;
					}

{FOR}				{
						yylval.keywordVal = strdup(yytext);
						return FOR;
					}

{FUNCTION}			{
						yylval.keywordVal = strdup(yytext);
						return FUNCTION;
					}

{RETURN}			{
						yylval.keywordVal = strdup(yytext);
						return RETURN;
					}

{BREAK}				{
						yylval.keywordVal = strdup(yytext);
						return BREAK;
					}

{CONTINUE}			{	
						yylval.keywordVal = strdup(yytext);
						return CONTINUE;
					}

{AND}				{
						yylval.keywordVal = strdup(yytext);
						return AND;
					}

{NOT}				{
						yylval.keywordVal = strdup(yytext);
						return NOT;
					}

{OR}				{
						yylval.keywordVal = strdup(yytext);
						return OR;
					}

{LOCAL}				{
						yylval.keywordVal = strdup(yytext);
						return LOCAL;
					}

{TRUE}				{
						yylval.keywordVal = strdup(yytext);
						return TRUE;
					}

{FALSE}				{
						yylval.keywordVal = strdup(yytext);
						return FALSE;
					}

{NIL}				{	
						yylval.keywordVal = strdup(yytext);
						return NIL;
					}


{EQUAL}				{
						yylval.operatorVal = strdup(yytext);
						return EQUAL;
					}

{PLUS}				{
						yylval.operatorVal = strdup(yytext);
						return PLUS;
					}
	
{MINUS}				{
						yylval.operatorVal = strdup(yytext);
						return MINUS;
					}

{ASTERISK}			{
						yylval.operatorVal = strdup(yytext);
						return ASTERISK;
					}

{DIVISION}			{
						yylval.operatorVal = strdup(yytext);
						return DIVISION;
					}

{MODULO}			{
						yylval.operatorVal = strdup(yytext);
						return MODULO;
					}

{COMPARISON}		{
						yylval.operatorVal = strdup(yytext);
						return COMPARISON;
					}

{UNEQUAL}			{
						yylval.operatorVal = strdup(yytext);
						return UNEQUAL;
					}

{PLUSPLUS}			{
						yylval.operatorVal = strdup(yytext);
						return PLUSPLUS;
					}

{MINUSMINUS}		{
						yylval.operatorVal = strdup(yytext);
						return MINUSMINUS;
					}

{GREATERTHAN}		{
						yylval.operatorVal = strdup(yytext);
						return GREATERTHAN;
					}

{LESSTHAN}			{
						yylval.operatorVal = strdup(yytext);
						return LESSTHAN;
					}

{GREATEREQUAL}		{
						yylval.operatorVal = strdup(yytext);
						return GREATEREQUAL;
					}

{LESSEQUAL}			{
						yylval.operatorVal = strdup(yytext);
						return LESSEQUAL;
					}		


{INTEGER}			{
						yylval.intVal = atoi(yytext);
						return INTEGER;
					}


{REAL}				{
						yylval.realVal = strtod(yytext, NULL);
						return REAL;
					}


{STRING}			{	unsigned int i = 0;
					unsigned int k = 0;
					char *output = malloc(strlen(yytext)+1);
					while (yytext[i] != '\0'){
						if(yytext[i] == '\\'){
							if(yytext[i+1] == 'n'){
								output[k] = '\n';
								i++;
							} else if (yytext[i+1] == 't'){
								output[k] = '\t';
								i++;
							} else if (yytext[i+1] == '\\'){
								output[k] = '\\';
								i++;
							}else if(yytext[i+1] == '\"'){
								output[k] = '\"';
								i++;
							} else {
								if(yytext[i + 1] != ' ' && yytext[i + 1] != '\n'){
									printf(RED "Invalid escape character in string at line: %u\n" RESET, yylineno);
									output[k] = yytext[i + 1];
									k++;
									i++;
								}

								i++;
								continue;
							}
						} else {
							output[k] = yytext[i];
						}
						
						i++;
						k++;
					}
					
					output[k - 1] = '\0';
					output = &(output[1]);

					yylval.stringVal = strdup(output);
					return STRING;
					}


{SQBRACKETOPEN}		{
						yylval.punctuationVal = strdup(yytext);
						return SQBRACKETOPEN;
					}

{SQBRACKETCLOSE}	{
						yylval.punctuationVal = strdup(yytext);
						return SQBRACKETCLOSE;
					}	

{CURBRACKETOPEN}	{
						yylval.punctuationVal = strdup(yytext);
						return CURBRACKETOPEN;
					}

{CURBRACKETCLOSE} 	{
						yylval.punctuationVal = strdup(yytext);
						return CURBRACKETCLOSE;
					}

{PARENTHOPEN}		{
						yylval.punctuationVal = strdup(yytext);
						return PARENTHOPEN;
					}

{PARENTHCLOSE}		{
						yylval.punctuationVal = strdup(yytext);
						return PARENTHCLOSE;
					}

{SEMICOLON}			{
						yylval.punctuationVal = strdup(yytext);
						return SEMICOLON;
					}

{COMMA}				{
						yylval.punctuationVal = strdup(yytext);
						return COMMA;
					}

{COLON}				{
						yylval.punctuationVal = strdup(yytext);
						return COLON;
					}

{DOUBLECOLON}		{
						yylval.punctuationVal = strdup(yytext);
						return DOUBLECOLON;
					}

{DOT}				{
						yylval.punctuationVal = strdup(yytext);
						return DOT;
					}

{DOUBLEDOT}			{
						yylval.punctuationVal = strdup(yytext);
						return DOUBLEDOT;
					}		


{IDENTIFIER}		{
						yylval.idVal = strdup(yytext);
						return IDENTIFIER;
					}


{SINGLECOM}			{ 		int c;
						do{
							c = input();
						}while(c != '\n' && c != EOF);
						
					}

{MULTICOM}			{		unsigned int multicomcounter = 1;
						int c;
						int start, end;
						unsigned int nestedComments = 0;
						start = yylineno;
					


						while ((c = input())){
							if(c == EOF)break;
							
							if(c == '/'){
								c = input();
								if(c == '*') {
									multicomcounter++;   /*nested comment*/
									nestedComments++;
								}
								else unput(c);
							} else if(c == '*'){
								c = input();
								if(c == '/'){
									
									multicomcounter--;
									if(multicomcounter <= 0) break;
								}
								else unput(c);
							}
						}
						end = yylineno;	
						
						if(multicomcounter > 0) fprintf(stderr, RED "Unclosed comment at line:%d\n" RESET, yylineno); 
							
						
					}


{WHITESPACE}		{
						
					}


{WRONGIDENT} 		{
						return WRONGIDENT;
					}
					
%%
