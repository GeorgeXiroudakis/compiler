%{
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "lib/symtable.h"
#include <string.h>


int yyerror (char* yyProvideMessage);
extern int alpha_yylex(void); 
extern int yylex(void); 

extern int yylineno;
extern char* yytext;
extern FILE* yyin;

int scopeLength = 0;
int scopeCapacity = 0;
int currScope = 0;

int anonymusFuncNum = 1;

SymTable_T symbolTable;
ScopeArray_t **ScopeLists = NULL;    //pinakas me listes me index ta scopes

/*our functions*/
void makeLibEntry(char *name);
void makeVariableEntry(char *name, enum SymbolType type);
void makeFuncEntry(char *name,enum SymbolType type);
void insertToScopeList(ScopeArray_t *head, scopeListNode_t *newNode);
void insertToSymTable(int scope, const char *name, SymbolTableEntry_t *newEntry);
FunctArgNode_t* makeFuncArgList(FunctArgNode_t* f,int scope);
SymbolTableEntry_t *upStreamLookUp(int scope,const char *key);
SymbolTableEntry_t *scopeLookUp(int scope,const char *key);
void hideScope(int scope);
void printEntry(const char *pcKey, void *pvValue, void *pvExtra);
void printFuncArgs(FunctArgNode_t *f);
void printScopeLists();
int libFuncCheck(char* key);

void allocateScopes(int scope);

/*phase 3*/
expr* emit_iftableitem(expr* e);
expr* member_item(expr* lvalue, char* name);

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
	struct	SymbolTableEntry* exprNode; 
}



%token<keywordVal> IF ELSE WHILE FOR FUNCTION RETURN BREAK CONTINUE AND NOT OR LOCAL TRUE FALSE NIL

%token<operatorVal> EQUAL PLUS MINUS ASTERISK DIVISION MODULO COMPARISON UNEQUAL PLUSPLUS MINUSMINUS GREATERTHAN LESSTHAN GREATEREQUAL LESSEQUAL

%token<punctuationVal> SQBRACKETOPEN SQBRACKETCLOSE CURBRACKETOPEN CURBRACKETCLOSE PARENTHOPEN PARENTHCLOSE SEMICOLON COMMA COLON DOT DOUBLEDOT DOUBLECOLON

%token<stringVal> STRING

%token<intVal> INTEGER

%token<doubleVal> REAL

%token<idVal> IDENTIFIER WRONGIDENT


/*To be checked*/
%type<idVal> lvalue member expr call /*Added dis just to compile :3*/




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

lvalue: IDENTIFIER		{
							if(libFuncCheck($1)){ 
								SymbolTableEntry_t *res = upStreamLookUp(currScope, $1);
								if(res!=NULL){
									if(res->type == libfunc )yyerror("Redifinition of token");
									else if(res->type == userfunc)yyerror("function used as an lvalue");
									else if(res->type != global){
										if(res->unionType == unionVar){
											if(res->value.varVal->scope != currScope){
												yyerror("Not accesible variable");
											}
										}/*else{
											if(res->value.funcVal->scope != currScope){
												yyerror("Not accesible function");
											}
										}*/
									}
								} 
								else{ (currScope == 0) ? makeVariableEntry($1,global) : makeVariableEntry($1,local);} 
							}else yyerror("existing library function with same name");
						}
						
     
	  | LOCAL IDENTIFIER	{ if(libFuncCheck($2)){ if(scopeLookUp(currScope, $2) == NULL)makeVariableEntry($2,local);} else yyerror("existing library function with same name"); }     
      | DOUBLECOLON IDENTIFIER	{(scopeLookUp(0,$2) == NULL) ? yyerror("Global Variable not found") : ($$ = $2);}
      | member
      ;

lvalue2: IDENTIFIER				{SymbolTableEntry_t *res = upStreamLookUp(currScope,$1);
								 if(res != NULL) {
								 	if(res->type != userfunc && res->type != libfunc) yyerror("Function not found");
									//else if(res->type == userfunc && res->value.funcVal->scope != currScope) yyerror("Not accesible function");
								 	else printf("caling function %s\n", res->value.varVal->name);
								 }else yyerror("Function not found");
						}		
	   | DOUBLECOLON IDENTIFIER	{SymbolTableEntry_t *res = upStreamLookUp(0,$2);
								 if(res != NULL){
									if(res->type != userfunc && res->type != libfunc) yyerror("Function not found");
									else printf("caling function %s\n", res->value.varVal->name);
								}else yyerror("Function not found");
					}
								
	   | member
	   ;

member: lvalue DOT IDENTIFIER
      | lvalue SQBRACKETOPEN expr SQBRACKETCLOSE
      | call DOT IDENTIFIER
      | call SQBRACKETOPEN expr SQBRACKETCLOSE
      ;

call: call PARENTHOPEN elist PARENTHCLOSE
    | lvalue2 callsuffix  
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
	 |
     ;

indexed: indexedelem
        | indexed COMMA indexedelem
        ;


objectdef: SQBRACKETOPEN objectdef_body SQBRACKETCLOSE;
objectdef_body: elist
              | indexed
			  ;





indexedelem: CURBRACKETOPEN expr COLON expr CURBRACKETCLOSE
	   ;

block: CURBRACKETOPEN {currScope++; allocateScopes(currScope);} stmt_list CURBRACKETCLOSE {hideScope(currScope);currScope--;}  
     ;

stmt_list: program
		 |
		 ;

/* stmt_list: stmt 
		 | stmt_list stmt
		 |
		 ;
*/

funcdef: FUNCTION IDENTIFIER {currScope++; allocateScopes(currScope); allocateScopes(currScope);} PARENTHOPEN idlist {currScope--;} PARENTHCLOSE {if(libFuncCheck($2)) makeFuncEntry($2,userfunc); else yyerror("existing library function with same name");} block
       | FUNCTION {int stringLength = snprintf(NULL,0,"%d",anonymusFuncNum);
       			char* functionName = (char*)malloc(strlen("_anonymusfunc") + stringLength + 1);
			snprintf(functionName,stringLength + 1 + strlen("_anonymusfunc"),"_anonymusfunc%d",anonymusFuncNum);
			makeFuncEntry(functionName,userfunc);anonymusFuncNum++;currScope++; allocateScopes(currScope);}PARENTHOPEN idlist {currScope--;} PARENTHCLOSE block 
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

idlist: IDENTIFIER {makeVariableEntry($1,formal);/*printf("Added Argument: %s\n",$1);*/}
      | idlist COMMA IDENTIFIER {makeVariableEntry($3,formal);/*printf("Added another Argument: %s\n",$3);*/}
      |
      ;

ifstmt: IF PARENTHOPEN expr PARENTHCLOSE stmt
      | IF PARENTHOPEN expr PARENTHCLOSE stmt ELSE stmt
      ;

whilestmt: WHILE PARENTHOPEN {currScope++; allocateScopes(currScope);} expr PARENTHCLOSE {currScope--;} stmt
	 ;

forstmt: FOR PARENTHOPEN {currScope++; allocateScopes(currScope); } elist SEMICOLON expr SEMICOLON elist PARENTHCLOSE {currScope--;} stmt
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


int yylex(void) {
    return alpha_yylex(); 
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
    	
    	/*[currScope+1] = malloc(sizeof(scopeListNode_t));
    	ScopeLists[currScope+1]->head = NULL;
    	ScopeLists[currScope+1]->tail = NULL;*/

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
    	

	printScopeLists();
	//SymTable_map(symbolTable,printEntry,NULL);

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

void makeFuncEntry(char *name,enum SymbolType type){
	SymbolTableEntry_t *entry;
	Function_t *f;
	
	f = malloc(sizeof(Function_t));

	f->name = name;
	f->scope = currScope;
	f->line = yylineno;
	f->arglist = NULL;

	f->arglist = makeFuncArgList(f->arglist,currScope);

	entry = malloc(sizeof(SymbolTableEntry_t));

	assert(entry);

	entry->isActive = 1;
	entry->unionType = unionFunc;
	entry->value.funcVal = f;
	entry->type = type;
	
	insertToSymTable(currScope,f->name,entry);
	printFuncArgs(f->arglist);
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


void allocateScopes(int scope){
	int oldCapacity = scopeCapacity;

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


}



/*pre-condition, the newEntry has not already been inserted in the symtable*/
void insertToSymTable(int scope, const char *name, SymbolTableEntry_t *newEntry){
    int oldCapacity = scopeCapacity;
    scopeListNode_t *p;
	SymbolTableEntry_t *duplicate = upStreamLookUp(scope, name);
	
	if(duplicate!=NULL && newEntry->type != local){
		if(duplicate->type == newEntry->type){
			/*in case of same type and scope, the id is referring to the existing entry*/
			if(duplicate->unionType == unionVar) if(duplicate->value.varVal->scope == scope) return;
			if(duplicate->unionType == unionFunc) if(duplicate->value.funcVal->scope == scope) {yyerror("redifinion of function"); return;}
			/*if the duplicate is a user function and the newEntry is a user function and the duplicate is at a bigger scope than the newEntry
			*the newEntry function refers to the function of the higher scope*/
			if(duplicate->type == userfunc && duplicate->value.funcVal->scope > scope) return;
		}
		/*in case the duplicate is a formal argument of the function, the newEntry refers to the existing argument*/
		if(duplicate->type == formal && newEntry->type == local) return; 		/*in case a var is used as a function*/
		if(duplicate->unionType != newEntry->unionType){yyerror("var redefined as a function"); return;}
	}


    /*inserting to symbol table*/
	if(SymTable_put(symbolTable, name, newEntry) == 0){
		if(newEntry->type != local)yyerror("redefinition of token");
		return;
	}
    p = malloc(sizeof(scopeListNode_t));

    p->entry = newEntry;
    p->next = NULL;
    insertToScopeList(ScopeLists[scope], p);
		
}


/*Function that adds the arguments of the function to the function list (scope is + 1 because the arguments have a + 1 scope unlike the function*/
FunctArgNode_t* makeFuncArgList(FunctArgNode_t* f, int scope){
	scopeListNode_t *p; 
	FunctArgNode_t* head = f;

	if(ScopeLists[scope + 1] == NULL) return f;

	p = ScopeLists[scope + 1]->head; 

	while(p != NULL){
		if((p->entry->isActive == 1) && (p->entry->type == formal)){
				FunctArgNode_t *newNode = malloc(sizeof(FunctArgNode_t));
				newNode->arg = p->entry;
				newNode->next = NULL;

				if(head == NULL){
					f = newNode;
					head = newNode;
				}else{
					head = f;
					while(head->next != NULL){
										
						head = head->next;
					}

					head->next = newNode;
				}

			
		}
		p = p->next;
	}

	return f;
}


/*Function that returns 0 if token is not found and 1 if found and var, 2 if found and func*/
SymbolTableEntry_t *upStreamLookUp(int scope, const char* key){
	SymbolTableEntry_t * res;
	res = scopeLookUp(scope, key);
	int ogScope = scope;
	if(res != NULL) return res;
	
	while (scope--){
		res = scopeLookUp(scope, key);
		if(res != NULL) return res;		
	}

	return NULL;
}


int libFuncCheck(char* key){
	scopeListNode_t *p;

	if(ScopeLists[0] == NULL) return 0;

	p = ScopeLists[0]->head;
	
	while(p != NULL){
		if( (p->entry->type == libfunc) && (strcmp(p->entry->value.funcVal->name,key) == 0) )return 0;

		p = p->next;
	}
	return 1;
}


/*Function that returns 0 if token is not found and 1 if found and var, 2 if found and func*/
SymbolTableEntry_t * scopeLookUp(int scope,const char* key){
	scopeListNode_t *p;
	
	if(ScopeLists[scope] == NULL) return NULL;

	p = ScopeLists[scope]->head;
	

	while(p != NULL){
		if(p->entry->unionType == unionVar){
			if( (p->entry->isActive == 1) && (strcmp(p->entry->value.varVal->name,key) == 0)){
				return p->entry;
			}
		}else{
			if( (p->entry->isActive == 1) && (strcmp(p->entry->value.funcVal->name,key) == 0)){
				return p->entry;
			}
		}
		p = p->next;
	}
	/*Not found*/
	return NULL;
}


/*Hides all tokens of the given scope*/
void hideScope(int scope){
    scopeListNode_t *p;
	
	if(ScopeLists[scope] == NULL ) return;
	
	p = ScopeLists[scope]->head;

	if(scope == 0){
		return;
	}

    	if(p == NULL){
    		return;
    	}

	while(p != NULL){
		p->entry->isActive = 0;
		p = p->next;
	}
}

void printEntry(const char *pcKey, void *pvValue, void *pvExtra){
    if( ((SymbolTableEntry_t *) pvValue)->isActive == 1){
	    printf("%s\n", pcKey);
    } else {
        printf("Hidden token: %s\n", pcKey);
    }
}

void printFuncArgs(FunctArgNode_t *f){
	FunctArgNode_t *args = f;
	
	while(args != NULL){
		printf("Function argument %s\n", args->arg->value.varVal->name);
		args = args->next;
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
				printf("(line %d) (scope %d)\n", p->entry->value.funcVal->line, p->entry->value.funcVal->scope);
			
				
			p = p->next;
		}
		printf("\n");
	}


	/*phase 3*/
	expr* emit_iftableitem(expr* e){
		if(e->type != tableitem_e)
			return e;
		else {
			expr* result = newexpr(var_e);
			result->sym = newtemp();
			emit(tablegetelem, e, e->index, result);
			return result;
		}
	}

	expr* member_item(expr* lvalue, char* name){
		lvalue = emit_iftableitem(lvalue);
		expr* item = newexpr(tableitem_e);
		item->sym = lvalue->sym;
		item->index = newexpr_conststring(name);
	}
}
