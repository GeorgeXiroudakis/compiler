%{
#include "alpha_parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YY_DECL int yylex(void)

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
BREAK			breakG
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

WRONGIDENT		{DIGIT}|_(_|{IDENTIFIER})+


%%

{IF}				{
						return IF;
					}

{ELSE}				{	
						return ELSE;
					}

{WHILE}				{
						return 	WHILE;
					}

{FOR}				{
						return FOR;
					}

{FUNCTION}			{
						return FUNCTION;
					}

{RETURN}			{
						return RETURN;
					}

{BREAK}				{
						return BREAK;
					}

{CONTINUE}			{	
						return CONTINUE;
					}

{AND}				{
						return AND;
					}

{NOT}				{
						return NOT;
					}

{OR}				{
						return OR;
					}

{LOCAL}				{
						return LOCAL;
					}

{TRUE}				{
						return TRUE;
					}

{FALSE}				{
						return FALSE;
					}

{NIL}				{	
						return NIL;
					}


{EQUAL}				{
						return EQUAL;
					}

{PLUS}				{
						return PLUS;
					}
	
{MINUS}				{
						return MINUS;
					}

{ASTERISK}			{
						return ASTERISK;
					}

{DIVISION}			{
						return DIVISION;
					}

{MODULO}			{
						return MODULO;
					}

{COMPARISON}		{
						return COMPARISON;
					}

{UNEQUAL}			{
						return UNEQUAL;
					}

{PLUSPLUS}			{
						return PLUSPLUS;
					}

{MINUSMINUS}		{
						return MINUSMINUS;
					}

{GREATERTHAN}		{
						return GREATERTHAN;
					}

{LESSTHAN}			{
						return LESSTHAN;
					}

{GREATEREQUAL}		{
						return GREATEREQUAL;
					}

{LESSEQUAL}			{
						return LESSEQUAL;
					}		


{DIGIT}				{
						return DIGIT;
					}


{REALDIGIT}			{
						return REALDIGIT;
					}


{STRING}			{
						return STRING;
					}


{SQBRACKETOPEN}		{
						return SQBRACKETOPEN;
					}

{SQBRACKETCLOSE}	{
						return SQBRACKETCLOSE;
					}	

{CURBRACKETOPEN}	{
						return CURBRACKETOPEN;
					}

{CURBRACKETCLOSE} 	{
						return CURBRACKETCLOSE;
					}

{PARENTHOPEN}		{
						return PARENTHOPEN;
					}

{PARENTHCLOSE}		{
						return PARENTHCLOSE;
					}

{SEMICOLON}			{
						return SEMICOLON;
					}

{COMMA}				{
						return COMMA;
					}

{COLON}				{
						return COLON;
					}

{DOUBLECOLON}		{
						return DOUBLECOLON;
					}

{DOT}				{
						return DOT;
					}

{DOUBLEDOT}			{
						return DOUBLEDOT;
					}		


{IDENTIFIER}		{
						return IDENTIFIER;
					}


{SINGLECOM}			{
						return SINGLECOM;
					}

{MULTICOM}			{
						return MULTICOM;
					}


{WHITESPACE}		{
						return WHITESPACE;
					}


{WRONGIDENT} 		{
						return WRONGIDENT;
					}
					
%%
