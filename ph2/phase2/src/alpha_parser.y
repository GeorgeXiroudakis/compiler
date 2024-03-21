%{
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "lib/symtable.h"
#include <string.h>


int yyerror (char* yyProvideMessage);
int yylex(void); 

extern int yylineno;
extern char* yytext;
extern FILE* yyin;

int scopeLength = 0;
int scopeCapacity = 0;
int currScope = 0;

SymTable_T symbolTable;
ScopeArray_t **ScopeLists;    //pinakas me listes me index ta scopes

/*our functions*/
void makeLibEntry(char *name);
void makeVariableEntry(char *name, enum SymbolType type);
void insertToScopeList(ScopeArray_t *head, scopeListNode_t *newNode);
void insertToSymTable(int scope, const char *name, SymbolTableEntry_t *newEntry);
void hideToken(char *name);
void printEntry(const char *pcKey, void *pvValue, void *pvExtra);
void printScopeLists();

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
    int line = 0;
	
    symbolTable = SymTable_new();
	ScopeLists = malloc(sizeof(ScopeArray_t *));
	assert(symbolTable || ScopeLists);
	
	scopeCapacity++;

	ScopeLists[currScope] = malloc(sizeof(scopeListNode_t));
    ScopeLists[currScope]->head = NULL;
    ScopeLists[currScope]->tail = NULL;

    makeLibEntry("print");
    makeLibEntry("input");
    makeLibEntry("objectmemberkeys");
    makeLibEntry("objecttotalmembers");
    makeLibEntry("objectcopy");
    makeLibEntry("totalarguments");
    makeLibEntry("argument");
    makeLibEntry("typeof");
    makeLibEntry("strtonum");
    makeLibEntry("sqrt");
    makeLibEntry("cos");
    makeLibEntry("sin");

    currScope++;
    makeLibEntry("tan");
    makeLibEntry("arctan");
    
    currScope++;
    makeLibEntry("Giwrgos");
    makeVariableEntry("Xiru", local);
    makeLibEntry("Tonis");
    makeLibEntry("Gaby");
	hideToken("print");
    makeLibEntry("print");

    currScope--;
    makeVariableEntry("Chadvidhs", global);
	makeVariableEntry("Chadvidhs", global);
    
    printScopeLists();

    
    SymTable_map(symbolTable, printEntry, NULL);
    

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

void makeLibEntry(char *name){

    SymbolTableEntry_t *entry;
    Function_t *f;

    f = malloc(sizeof(Function_t));

	f->name = name;
	f->scope = currScope;
	f->line = 0;

    entry = malloc(sizeof(SymbolTableEntry_t));

    assert(entry);

	entry->isActive = 1;
	entry->unionType = unionFunc;
	entry->value.funcVal = f;
	entry->type = libfunc;

    insertToSymTable(currScope, f->name, entry);

}

void makeVariableEntry(char *name, enum SymbolType type){
    SymbolTableEntry_t *entry;
    Variable_t *v;

    v = malloc(sizeof(Variable_t));

	v->name = name;
	v->scope = currScope;
	v->line = yylineno;

    entry = malloc(sizeof(SymbolTableEntry_t));

    assert(entry);

	entry->isActive = 1;
	entry->unionType = unionVar;
	entry->value.varVal = v;
	entry->type = type;

    insertToSymTable(currScope, v->name, entry);
}

void insertToScopeList(ScopeArray_t *scope, scopeListNode_t *newNode){
	
	if(newNode == NULL) {
		perror("newNode == NULL\n");
	}	
	
	if(scope->head == NULL){
		scope->head = newNode;
        scope->tail = newNode;
		return;
	}

	assert(scope->tail);
	scope->tail->next = newNode;
    scope->tail = newNode;

}


/*pre-condition, the newEntry has not already been inserted in the symtable*/
void insertToSymTable(int scope, const char *name, SymbolTableEntry_t *newEntry){
    int oldCapacity = scopeCapacity;
    scopeListNode_t *p;

    /*inserting to symbol table*/
	if(SymTable_put(symbolTable, name, newEntry) == 0){
		yyerror("redefinition of token");
		return;
	}

    /*inserting to scope list*/
    while(scopeCapacity <= scope){       /*if the scope is outside of arrays length reallocate space*/
        scopeCapacity *= 2;
        ScopeLists = realloc(ScopeLists, scopeCapacity * sizeof(ScopeArray_t *));

        for(int i = oldCapacity; i < scopeCapacity; i++){
        	ScopeLists[i] = malloc(sizeof(scopeListNode_t *));
        	ScopeLists[i]->head = NULL;
        	ScopeLists[i]->tail = NULL;
        }

        oldCapacity = scopeCapacity;
    }

    if(scopeLength < scope){
        scopeLength = scope;
    }

    p = malloc(sizeof(scopeListNode_t));

    p->entry = newEntry;
    p->next = NULL;
    insertToScopeList(ScopeLists[scope], p);
		
}


void hideToken(char *name){
    SymbolTableEntry_t *tokenToHide;

    tokenToHide = SymTable_get(symbolTable, name);

    tokenToHide->isActive = 0;
}

void printEntry(const char *pcKey, void *pvValue, void *pvExtra){
    if( ((SymbolTableEntry_t *) pvValue)->isActive == 1){
	    printf("%s\n", pcKey);
    } else {
        //printf("Hidden token: %s\n", pcKey);
    }
}

void printScopeLists(){
	scopeListNode_t *p;
	
	for(int i = 0; i < scopeLength+1; i++){
		printf("-----------      Scope #%d      -----------\n", i);
		p = ScopeLists[i]->head;
		while(p != NULL){
			printf("\"%s\" ", p->entry->value.funcVal->name);
			switch (p->entry->type){
				case global:
					printf("[global variable] ");
					break;
				case local:
					printf("[local variable] ");
					break;
				case formal:
					printf("[formal argument] ");
					break;
				case userfunc:
					printf("[user function] ");
					break;
				case libfunc:
					printf("[library function] ");
					break;
				default:
					printf("[unknown type] ");
					break;
			}
			
			if(p->entry->unionType == unionVar)
				printf("(line %d) (scope %d)\n", p->entry->value.varVal->line, p->entry->value.varVal->scope);
			else 
				printf("(line %d) (scope %d)\n", p->entry->value.varVal->line, p->entry->value.varVal->scope);
			
				
			p = p->next;
		}
		printf("\n");
	}
}


