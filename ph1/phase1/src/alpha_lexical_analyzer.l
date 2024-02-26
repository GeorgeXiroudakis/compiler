%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YY_DECL int alpha_yylex (void* yylval)

#define RESET   "\033[0m"
#define RED     "\033[31m"

unsigned int tokencounter = 1;

typedef struct alpha_token_t{
  unsigned int     numline;
  unsigned int     numToken;
  char	           *content;
  char             *type;
  char		   *specificContent;
  char		   *vartype;
  struct           alpha_token_t *next;
}alpha_token_t;

void insertAlphaToken(alpha_token_t **head, alpha_token_t *newNode);
void addToTokenList(char *typename, char *vartypename, void *yylval, char *spCon);

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

WRONGIDENT		{DIGIT}|_(_|{IDENTIFIER})+


%%

{IF}				{
						addToTokenList("KEYWORD", "enumerated", yylval, "IF");
					}


{ELSE}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "ELSE");
					}


{WHILE}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "WHILE");
					}


{FOR}				{ 
						addToTokenList("KEYWORD", "enumerated", yylval, "FOR");
					}


{FUNCTION}			{  
						addToTokenList("KEYWORD", "enumerated", yylval, "FUNCTION");	
					}


{RETURN}			{  
						addToTokenList("KEYWORD", "enumerated", yylval, "RETURN");
					}


{BREAK}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "BREAK");
					}


{CONTINUE}			{  
					addToTokenList("KEYWORD", "enumerated", yylval, "CONTINUE");
					}


{AND}				{ 
						addToTokenList("KEYWORD", "enumerated", yylval, "AND");
					}


{NOT}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "NOT");
					}


{OR}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "OR");
					}


{LOCAL}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "LOCAL");
					}


{TRUE}				{  

						addToTokenList("KEYWORD", "enumerated", yylval, "TRUE");
				    }


{FALSE}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "FALSE");
					}


{NIL}				{  
						addToTokenList("KEYWORD", "enumerated", yylval, "NIL");		
					}


{EQUAL}        		{  
						addToTokenList("OPERATOR", "enumerated", yylval, "EQUAL");
					}


{PLUS}      		{ 
						addToTokenList("OPERATOR", "enumerated", yylval, "PLUS");
					}


{MINUS}      		{ 
						addToTokenList("OPERATOR", "enumerated", yylval, "MINUS");
					}


{ASTERISK}     		{  
						addToTokenList("OPERATOR", "enumerated", yylval, "ASTERISK");
					}


{DIVISION}          {  
						addToTokenList("OPERATOR", "enumerated", yylval, "DIVISION");
					}


{MODULO}    		{  
						addToTokenList("OPERATOR", "enumerated", yylval, "MODULO");
					}


{COMPARISON}		{  
						addToTokenList("OPERATOR", "enumerated", yylval, "COMPARISON");
					}


{UNEQUAL}   		{  
						addToTokenList("OPERATOR", "enumerated", yylval, "UNEQUAL");
					}


{PLUSPLUS}    		{  
						addToTokenList("OPERATOR", "enumerated", yylval, "PLUSPLUS");
					}


{MINUSMINUS}    	{  
						addToTokenList("OPERATOR", "enumerated", yylval, "MINUSMINUS");
					}


{GREATERTHAN}   	{ 
						addToTokenList("OPERATOR", "enumerated", yylval, "GREATERTHAN");
					}


{LESSTHAN}      	{  
						addToTokenList("OPERATOR", "enumerated", yylval, "LESSTHAN");
					} 


{GREATEREQUAL}      {  
						addToTokenList("OPERATOR", "enumerated", yylval, "GREATERQUAL");
					}


{LESSEQUAL}         {  
						addToTokenList("OPERATOR", "enumerated", yylval, "LESSEQUAL");
					}


{DIGIT}			    {  
						char *temp;
						temp = malloc(strlen(yytext) + 1);
						strcpy(temp, yytext);
						addToTokenList("CONST_INT", "integer", yylval, temp);
					}


{REALDIGIT}			{  
						char *temp;
						temp = malloc(strlen(yytext) + 1);
						strcpy(temp, yytext);
						addToTokenList("REAL_DIGIT", "real", yylval, temp);
					}


{STRING}			{ 
					
					unsigned int i = 0;
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

					alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
					newNode->numline = yylineno;
					newNode->numToken = tokencounter++;
					newNode->content = malloc(strlen(output)+1);
					strcpy(newNode->content, output);
					newNode->type = "STRING";
					newNode->next = NULL;
					newNode->vartype = "char *";
					newNode->specificContent = output;
					
					insertAlphaToken((alpha_token_t **)yylval, newNode);
					}


{SQBRACKETOPEN}     {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "SQBRACKETOPEN");
					}


{SQBRACKETCLOSE}    { 
						addToTokenList("PUNCTUATION", "enumerated", yylval, "SQBRACKETCLOSE");
					}


{CURBRACKETOPEN}    { 
			  			addToTokenList("PUNCTUATION", "enumerated", yylval, "CURBRACKETOPEN");
					}


{CURBRACKETCLOSE}   {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "CURBRACKETCLOSE");
					}


{PARENTHOPEN}		{ 
						addToTokenList("PUNCTUATION", "enumerated", yylval, "PARENTHOPEN");
					}


{PARENTHCLOSE}      {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "PARENTHCLOSE");
					}	


{SEMICOLON}         {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "SEMICOLON");
					}		


{COMMA}             {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "COMMA");
					}


{COLON}             {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "COLON");
					}


{DOUBLECOLON}       {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "DOUBLECOLON");
					}		


{DOT}				{  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "DOT");
					}			


{DOUBLEDOT}         {  
						addToTokenList("PUNCTUATION", "enumerated", yylval, "DOUBLEDOT");
					}		


{IDENTIFIER}		{  
						char *temp;
						temp = malloc(strlen(yytext)+1);
						strcpy(temp, yytext);
						addToTokenList("IDENTIFIER", "char *", yylval, temp);			
					}


{SINGLECOM}         {
						int c;
						do{
							c = input();
						}while(c != '\n' && c != EOF);

							alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						
							newNode->numline = yylineno;
							newNode->numToken = tokencounter++;
							newNode->content = "";
							newNode->type = "COMMENT";
							newNode->vartype = "enumerated";
							newNode->specificContent = "LINE_COMMENT";
							newNode->next = NULL;
	
							insertAlphaToken((alpha_token_t **)yylval, newNode);
					}


{MULTICOM}			{
						unsigned int multicomcounter = 1;
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
						else{
							alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
							
							int length = snprintf(NULL, 0, "%d - %d with %d nested comments", start, end, nestedComments) + 1;
							newNode->content = (char *)malloc(length * sizeof(char));

							newNode->numline = start;
							newNode->numToken = tokencounter++;
							snprintf(newNode->content, length, "%d - %d with %d nested comments", start, end, nestedComments);
							newNode->type = "COMMENT";
							newNode->vartype = "enumerated";
							newNode->specificContent = "BLOCK_COMMENT";
							newNode->next = NULL;
	
							insertAlphaToken((alpha_token_t **)yylval, newNode);

						}
					}


{WHITESPACE}		{ ; }


{WRONGIDENT}		{ printf("Wrong ID: %s\n", yytext); fprintf(stderr, RED "Invalid identifier %s at line:%d\n" RESET, yytext, yylineno); exit(-1);}


.					{ printf(RED "Unexpected character: %s  at line:%d\n" RESET , yytext, yylineno); }
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

void addToTokenList(char *typename, char *vartypename, void *yylval, char *spCon){
	alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
	
	newNode->numline = yylineno;
	newNode->numToken = tokencounter++;
	newNode->content = malloc(strlen(yytext) + 1);
	strcpy(newNode->content, yytext);
	newNode->type = typename;
	newNode->vartype = vartypename;
	newNode->specificContent = spCon;
	newNode->next = NULL;
	
	insertAlphaToken((alpha_token_t **)yylval, newNode);
	
}

void printTokenList(alpha_token_t **head){
	alpha_token_t *q = *head;

	while(q != NULL){
		if( (strcmp(q->type, "IDENTIFIER") == 0) || strcmp(q->type, "STRING") == 0){
			printf("%u: #%u \"%s\" %s \"%s\" <-- %s\n", q->numline, q->numToken, q->content, q->type, q->specificContent, q->vartype);
		}else{
			printf("%u: #%u \"%s\" %s %s <-- %s\n", q->numline, q->numToken, q->content, q->type, q->specificContent, q->vartype);
		}
		q = q->next;
	}
}



int main() {
    alpha_token_t *yylval = NULL;
	alpha_yylex (&yylval);

	printTokenList(&yylval);
    return 0;
}

