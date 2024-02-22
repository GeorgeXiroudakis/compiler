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
  char		   *vartype;
  struct           alpha_token_t *next;
}alpha_token_t;

void insertAlphaToken(alpha_token_t **head, alpha_token_t *newNode);
void addToTokenList(char *typename, char *vartypename, void *yylval);

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

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{ELSE}				{ printf("ELSE: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{WHILE}				{ printf("WHILE: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{FOR}				{ printf("FOR: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{FUNCTION}			{ printf("FUNCTION: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);	
				}


{RETURN}			{ printf("RETURN: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{BREAK}				{ printf("BREAK: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{CONTINUE}			{ printf("CONTINUE: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{AND}				{ printf("AND: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{NOT}				{ printf("NOT: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{OR}				{ printf("OR: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{LOCAL}				{ printf("LOCAL: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{TRUE}				{ printf("TRUE: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{FALSE}				{ printf("FALSE: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);
				}


{NIL}				{ printf("NIL: %s\n", yytext); 

					addToTokenList("KEYWORD", "enumerated", yylval);		
				}


{EQUAL}             		{ printf("EQUAL: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{PLUS}              		{ printf("PLUS: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{MINUS}             		{ printf("MINUS: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{ASTERISK}          		{ printf("ASTERISK: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{DIVISION}          		{ printf("DIVISION: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{MODULO}            		{ printf("MODULO: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{COMPARISON}        		{ printf("COMPARISON: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{UNEQUAL}           		{ printf("UNEQUAL: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{PLUSPLUS}          		{ printf("PLUSPLUS: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{MINUSMINUS}        		{ printf("MINUSMINUS: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{GREATERTHAN}       		{ printf("GREATERTHAN: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{LESSTHAN}          		{ printf("LESSTHAN: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{GREATEREQUAL}      		{ printf("GREATEREQUAL: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{LESSEQUAL}         		{ printf("LESSEQUAL: %s\n", yytext); 

					addToTokenList("OPERATOR", "enumerated", yylval);
				}


{DIGIT}				{ printf("DIGIT: %s\n", yytext); 

					addToTokenList("CONST_INT", "integer", yylval);
				}


{REALDIGIT}			{ printf("REALDIGIT: %s\n", yytext); 
					
					addToTokenList("REAL_DIGIT", "real", yylval);
				}


{STRING}			{ printf("STRING: %s\n", yytext); 
					
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
								i++;
								continue;
							}
						} else {
							output[k] = yytext[i];
						}
						
						i++;
						k++;
					}
					
					output[k] = '\0';
					
					alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
					newNode->numline = yylineno;
					newNode->numToken = tokencounter++;
					newNode->content = malloc(strlen(output)+1);
					strcpy(newNode->content, output);
					newNode->type = "STRING";
					newNode->next = NULL;
					
					insertAlphaToken((alpha_token_t **)yylval, newNode);
									
				}


{SQBRACKETOPEN}     { printf("SQBRACKETOPEN: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
		    }


{SQBRACKETCLOSE}    { printf("SQBRACKETCLOSE: %s\n", yytext); 
			
						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
		    }


{CURBRACKETOPEN}    { printf("CURBRACKETOPEN: %s\n", yytext); 
			  			
			  			alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
		    }


{CURBRACKETCLOSE}   { printf("CURBRACKETCLOSE: %s\n", yytext); 
			
						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
		    }


{PARENTHOPEN}		{ printf("PARENTHOPEN: %s\n", yytext); 

				alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
			}


{PARENTHCLOSE}      { printf("PARENTHCLOSE: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
	            }


{SEMICOLON}         { printf("SEMICOLON: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
		    }


{COMMA}             { printf("COMMA: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
 		    }


{COLON}             { printf("COLON: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
		  }


{DOUBLECOLON}       { printf("DOUBLECOLON: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
		    }


{DOT}		{ printf("DOT: %s\n", yytext); 
			
						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);

		}


{DOUBLEDOT}         { printf("DOUBLEDOT: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "PUNCTUATION";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);
						
		    }


{IDENTIFIER}		{ printf("IDENTIFIER: %s\n", yytext); 

						alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
						newNode->numline = yylineno;
						newNode->numToken = tokencounter++;
						newNode->content = malloc(strlen(yytext) + 1);
						strcpy(newNode->content, yytext);
						newNode->type = "IDENTIFIER";
						newNode->next = NULL;
						
						insertAlphaToken((alpha_token_t **)yylval, newNode);			
			}


{SINGLECOM}         {
						printf("SINGELECOM: %s\n", yytext); 

						int c;
						do{
							c = input();
						}while(c != '\n' && c != EOF);

					}


{MULTICOM}			{
						printf("MULTICOM: %s\n", yytext);
						
						unsigned int multicomcounter = 1;
						int c;
						while ((c = input())){
							if(c == EOF)break;
							
							if(c == '/'){
								c = input();
								if(c == '*') multicomcounter++;   /*nested comment*/
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
						
						
						if(multicomcounter > 0) fprintf(stderr, RED "Unclosed comment at line:%d\n" RESET, yylineno); 

					}


{WHITESPACE}		{ printf("WHITESPACE\n"); }


{WRONGIDENT}		{ printf("Wrong ID: %s\n", yytext); fprintf(stderr, RED "Invalid identifier %s at line:%d\n" RESET, yytext, yylineno); exit(-1);}


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

void addToTokenList(char *typename, char *vartypename, void *yylval){
	alpha_token_t *newNode = malloc(sizeof(alpha_token_t));
	
	newNode->numline = yylineno;
	newNode->numToken = tokencounter++;
	newNode->content = malloc(strlen(yytext) + 1);
	strcpy(newNode->content, yytext);
	newNode->type = typename;
	newNode->vartype = vartypename;
	newNode->next = NULL;
	
	insertAlphaToken((alpha_token_t **)yylval, newNode);
	
}

void printTokenList(alpha_token_t **head){
	alpha_token_t *q = *head;

	while(q != NULL){
		if(q->type != "STRING"){
			printf("%u: #%u \"%s\" %s\n", q->numline, q->numToken, q->content, q->type);
		} else {
			printf("%u: #%u %s %s\n", q->numline, q->numToken, q->content, q->type);
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

