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
int anonymusFuncNum = 0;

SymTable_T symbolTable;
ScopeArray_t **ScopeLists = NULL;

/*our functions*/
SymbolTableEntry_t* makeLibEntry(char *name);
SymbolTableEntry_t* makeVariableEntry(char *name, enum SymbolType type);
SymbolTableEntry_t* makeFuncEntry(char *name,enum SymbolType type);
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

struct quad* quads = (struct quad*)0;
unsigned total = 0;
unsigned int currQuad = 0;

int tempcounter = 0;
int scopeoffset = 0;

//sori
short int elistFlag = 0;
short int indexedFlag = 0; 

unsigned programVarOffset = 0;
unsigned functionLocalOffset = 0;
unsigned formalArgOffset = 0;
unsigned scopeSpaceCounter = 1;

struct lc_stack_t* lcs_top = 0;
struct lc_stack_t* lcs_bottom = 0;

int infunction = 0;

#define loopcounter (lcs_top->counter)

struct expr* emit_iftableitem(struct expr* e);
struct expr* member_item(struct expr* lvalue, char* name);
struct expr* newexpr_conststring(char* c);
SymbolTableEntry_t* newtemp();

void emit(
	enum iopcode op,
	struct expr* arg1,
	struct expr* arg2,
	struct expr* result,
	unsigned label,
	unsigned line
	);

void checkArithmetic(struct expr* e);
enum scope_space currscopespace(void);
unsigned currscopeoffset(void);
void inccurrscopeoffset(void);
void enterscopespace(void);
void exitscopespace(void);
void resetformalargsoffset(void);
void resetfunctionlocalsoffset(void);
void  restorecurrscopeoffset(unsigned n);
unsigned nextquadlabel(void);
void patchlabel(unsigned quadNo,unsigned label);
void printQuads(void);
struct expr* newexpr_constnum(int value);
struct expr* newexpr_constbool(short int value);
struct expr* makeExpression(enum expr_en type, SymbolTableEntry_t* sym, struct expr* index, struct expr* next);  
struct expr* makeCall(struct expr* lv,struct exprNode* head);
struct exprNode* reverseList(struct exprNode* head);
unsigned int istempname(char* s);
unsigned int istempexpr(struct expr* e);
void push_loopcounter(void);
void pop_loopcounter(void);
struct stmt_t* make_stmt();
int newlist(int  i);
int mergelist(int l1,int l2);
void patchlist(int list,unsigned label);
void printStack(int list);

%}

%start program

%union{ 
	enum iopcode* op;
	char* 	keywordVal;
	char* 	punctuationVal;
    	char*	stringVal;	
	char*	idVal;
	char* 	operatorVal;
	unsigned unsignedVal;
	int 	intVal;
	double	realVal;
	struct SymbolTableEntry* entryNode;
	struct expr* exprNode;
 	struct exprNode* exprList;
	struct FunctArgNode* argNode;
 	struct call* callType;
	struct indexed_elem* indexedType;
	struct for_labels* forLabelsType;
	struct stmt_t* stmtType;
}



%token<keywordVal> IF ELSE WHILE FOR FUNCTION RETURN BREAK CONTINUE AND NOT OR LOCAL TRUE FALSE NIL

%token<operatorVal> PLUSPLUS MINUSMINUS EQUAL PLUS MINUS ASTERISK DIVISION MODULO COMPARISON UNEQUAL GREATERTHAN LESSTHAN GREATEREQUAL LESSEQUAL

%token<punctuationVal> SQBRACKETOPEN SQBRACKETCLOSE CURBRACKETOPEN CURBRACKETCLOSE PARENTHOPEN PARENTHCLOSE SEMICOLON COMMA COLON DOT DOUBLEDOT DOUBLECOLON

%token<stringVal> STRING

%token<intVal> INTEGER

%token<realVal> REAL

%token<idVal> IDENTIFIER WRONGIDENT



%type<op> arithop compop boolop

%type<entryNode>   number /*:3*/

%type<exprNode> call expr term primary const assignexpr lvalue call_lvalue member objectdef

%type<exprList> elist

%type<idVal> funcname

%type<unsignedVal> funcbody ifprefix elseprefix whilestart whilecond unfinjmp forretlabel

%type<entryNode> funcdef funcprefix

%type<argNode> funcargs

%type<callType> methodcall normcall callsuffix

%type<indexedType> indexed indexedelem

%type<forLabelsType> forprefix forpostfix

%type<stmtType> stmt loopstmt break continue stmts while for if returnstmt block





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

stmt: expr SEMICOLON { $$ = make_stmt($$); }
    | if { $$ = make_stmt($$); }
    | while {$$ =  make_stmt($$); }
    | for { $$ = make_stmt($$); }
    | returnstmt {$$ =  make_stmt($$); }
    | break  { $$ = make_stmt($$); }
    | continue { $$ = make_stmt($$); }
    | block { $$ = make_stmt($$); }
    | funcdef /*{ $$ = make_stmt($1); }*/
    | SEMICOLON
    ;

break: BREAK SEMICOLON {
			struct stmt_t* temp;
			make_stmt($$);
			temp = $$;
			$$->breakList = newlist(nextquadlabel());
			temp = $$;
			emit(JUMP,NULL,NULL,NULL,0,0);
		       }

continue: CONTINUE SEMICOLON {
			      make_stmt($$);
			      $$->contList = newlist(nextquadlabel());
			      emit(JUMP,NULL,NULL,NULL,0,0);
			     }

stmts: stmts stmt{
                   	 struct stmt_t* temp;
			 //$$ = $2;
		   	 $$->breakList = mergelist($1->breakList,$$->breakList);
		  	 $$->contList = mergelist($1->contList,$$->contList);
			 printStack($$->contList);
		  	 temp = $$;
		  	 printf("sto allo 2\n");
		     }

     |stmt { $$ = $1;
     	     printf("sto ena ena\n");
	    }  
     ;



expr: assignexpr {	struct expr* temp;
    			$$ = $1;
			temp = $$;
		 }
    | expr arithop expr %prec PLUS {
				if($1->sym->gramType != gr_integer && $1->sym->gramType != gr_constinteger && $1->sym->gramType != gr_constreal && $1->sym->gramType != gr_real){
					yyerror("Invalid type for arithmetic expression");
				}else if($3->sym->gramType != gr_integer && $3->sym->gramType != gr_constinteger && $3->sym->gramType != gr_constreal && $3->sym->gramType  != gr_real){
					yyerror("Invalid type for arithmetic expression");
				}else{	
					$$ = makeExpression(arithexpr_e,newtemp(),NULL,NULL);
					emit(*$2,$1,$3,$$,0,0);
				}
				   }
    | expr compop expr %prec GREATERTHAN {$$ = makeExpression(boolexpr_e,newtemp(),NULL,NULL);
    					  
					  if($1->sym->gramType != gr_integer && $1->sym->gramType != gr_constinteger && $1->sym->gramType != gr_constreal && $1->sym->gramType != gr_real){
						yyerror("Invalid type for compare expression");
				   }else if($3->sym->gramType != gr_integer && $3->sym->gramType != gr_constinteger && $3->sym->gramType != gr_constreal && $3->sym->gramType  != gr_real){
						yyerror("Invalid type for compare expression");
					}else{
    					
    					  emit(*$2,$1,$3,NULL,nextquadlabel() + 3,0);
					  emit(ASSIGN,newexpr_constbool(0),NULL,$$,0,0);
					  emit(JUMP,NULL,NULL,NULL,nextquadlabel() + 2,0);
					  emit(ASSIGN,newexpr_constbool(1),NULL,$$,0,0);
					}
					 }
    | expr boolop expr %prec AND {$$ = makeExpression(boolexpr_e,newtemp(),NULL,NULL);
    				  if($1->sym->gramType != gr_boolean && $3->sym->gramType != gr_boolean){
				  	yyerror("Invalid type for boolean expression");
				  }else{
				  	emit(*$2,$1,$3,$$,0,0);
				  }
				 }
    | term { $$ = makeExpression($1->type,$1->sym,NULL,NULL); }
    ;

compop:   GREATERTHAN{$$ = malloc(sizeof(enum iopcode)); *$$ = IF_GREATER;}
  	| GREATEREQUAL{$$ = malloc(sizeof(enum iopcode)); *$$ = IF_GREATEREQ;}
  	| LESSTHAN{$$ = malloc(sizeof(enum iopcode)); *$$ = IF_LESS;}
  	| LESSEQUAL{$$ = malloc(sizeof(enum iopcode)); *$$ = IF_LESSEQ;}
	| COMPARISON{$$ = malloc(sizeof(enum iopcode)); *$$ = IF_EQ;}
	| UNEQUAL{$$ = malloc(sizeof(enum iopcode)); *$$ = IF_NOTEQ;}
 	;

arithop: PLUS {$$ = malloc(sizeof(enum iopcode)); *$$ = ADD;}
  	| MINUS {$$ = malloc(sizeof(enum iopcode)); *$$ = SUB;}
  	| ASTERISK{$$ = malloc(sizeof(enum iopcode)); *$$ = MUL;}
  	| DIVISION{$$ = malloc(sizeof(enum iopcode)); *$$ = DIV;}
  	| MODULO{$$ = malloc(sizeof(enum iopcode)); *$$ = MOD;}
        ;

boolop:   AND{$$ = malloc(sizeof(enum iopcode)); *$$ = OP_AND;}
	| OR{$$ = malloc(sizeof(enum iopcode)); *$$ = OP_OR;}
        ;


term: PARENTHOPEN expr PARENTHCLOSE {$$ = $2;}
    | MINUS expr {checkArithmetic($2);
    		  $$ = makeExpression(arithexpr_e,newtemp(),NULL,NULL);
		  emit(UMINUS,
		  $2,
		  NULL,
		  $$,
		  0,
		  $2->sym->value.varVal->line);
		 }
    | NOT expr {
    	       	$$ = makeExpression(boolexpr_e,newtemp(),NULL,NULL);
		emit(OP_NOT,$2,NULL,$$,0,$2->sym->value.varVal->line);
	       }
    | PLUSPLUS lvalue {checkArithmetic($2);
		       	if($2->type == tableitem_e){
				$$ = emit_iftableitem($2);
				emit(ADD,$$,newexpr_constnum(1),$$,0,0);
				emit(TABLESETELEM,$2,$2->index,$$,0,0);
			}else{
				emit(ADD,$2,newexpr_constnum(1),$2,0,$2->sym->value.varVal->line);
				$$ = makeExpression(arithexpr_e,$2,NULL,NULL);
				$$->sym = newtemp();
				emit(ASSIGN,$2,NULL,$$,0,$2->sym->value.varVal->line);
			}
		      }
    | lvalue PLUSPLUS {
    			 checkArithmetic($1);
			 $$ = makeExpression(arithexpr_e,$2,NULL,NULL);
			 $$->sym = newtemp();

			 if($1->type == tableitem_e){
				struct expr* val = emit_iftableitem($1);
				emit(ASSIGN,val,NULL,$$,0,0);
				emit(ADD,val,newexpr_constnum(1),val,0,0);
				emit(TABLESETELEM,$1,$1->index,val,0,0);

			 }else{
				emit(ASSIGN,$1,NULL,$$,0,$1->sym->value.varVal->line);
			 	emit(ADD,$1,newexpr_constnum(1),$1,0,$1->sym->value.varVal->line);
			}
    			
    		      }
    | MINUSMINUS lvalue { 
    			  checkArithmetic($2); 
			 
			  if($2->type == tableitem_e){
				 $$ = emit_iftableitem($2);
				emit(SUB,$$,newexpr_constnum(1),$$,0,0);
				emit(TABLESETELEM,$2,$2->index,$$,0,0);
			  }else{
				 emit(SUB,$2,newexpr_constnum(1),$2,0,0);
				 $$ = makeExpression(arithexpr_e,$2,NULL,NULL);
				 $$->sym = newtemp();
				 emit(ASSIGN,$2,NULL,$$,0,$2->sym->value.varVal->line);
			 }
			}
    | lvalue MINUSMINUS {
    			 checkArithmetic($1);
			 $$ = makeExpression(arithexpr_e,$2,NULL,NULL);
			 $$->sym = newtemp();
			
			 	if($1->type == tableitem_e){
					struct expr* val = emit_iftableitem($1);
					emit(ASSIGN,val,NULL,$$,0,0);
					emit(SUB,val,newexpr_constnum(1),val,0,0);
					emit(TABLESETELEM,$1,$1->index,val,0,0);
				}else{
			 		emit(ASSIGN,$1,NULL,$$,0,$1->sym->value.varVal->line);
			 		emit(SUB,$1,newexpr_constnum(1),$1,0,$1->sym->value.varVal->line);
				}
			}
    | primary {$$ = makeExpression($1->type,$1->sym,NULL,NULL);}
    ;

assignexpr: lvalue EQUAL expr {
								
								if($1->sym->type == userfunc || $1->sym->type == libfunc){
									yyerror("function used as an lvalue");
								}
								else{
										if($1->type == tableitem_e){
											emit(TABLESETELEM,$1,$1->index,$3,0,0);
											$$ = emit_iftableitem($1);
											$$->type = assignexpr_e;
										}else{
											SymbolTableEntry_t* t = $1->sym;
											SymbolTableEntry_t* t2 = $3->sym;
											$1->sym->gramType = $3->sym->gramType;
											if($3->sym->gramType == gr_conststring) $1->sym->grammarVal.string = strdup($3->sym->grammarVal.string);
											else if($3->sym->gramType == gr_string) $1->sym->grammarVal.string = strdup($3->sym->grammarVal.string);
											else if($3->sym->gramType == gr_boolean) $1->sym->grammarVal.boolean = $3->sym->grammarVal.boolean;
											else if($3->sym->gramType == gr_nil) $1->sym->grammarVal.nil = $3->sym->grammarVal.nil;
											else if($3->sym->gramType == gr_integer) $1->sym->grammarVal.intNum = $3->sym->grammarVal.intNum;
											else if($3->sym->gramType == gr_constinteger) $1->sym->grammarVal.intNum = $3->sym->grammarVal.intNum;
											else if($3->sym->gramType == gr_real) $1->sym->grammarVal.realNum = $3->sym->grammarVal.realNum;
											else if($3->sym->gramType == gr_constreal) $1->sym->grammarVal.realNum = $3->sym->grammarVal.realNum;
											else if($3->sym->gramType == gr_funcaddr){printf("tt\n"); $1->sym->grammarVal.funcPtr = $3->sym->grammarVal.funcPtr;}
											$1->type = $3->type;
											emit(ASSIGN,$3, NULL, $1,0,0);
											$$ = makeExpression(assignexpr_e,$1->sym,NULL,NULL); 
											$$->sym = newtemp();
											$$->sym->gramType = $3->sym->gramType;
											emit(ASSIGN, $1, NULL, $$, 0, 0);
									   }
								}
							}
	  ;

primary: lvalue {$$ = emit_iftableitem($1);}
       | call
       | objectdef  {$$ = makeExpression($1->type,$1->sym,NULL,NULL);} 
       | PARENTHOPEN funcdef PARENTHCLOSE { $$ = makeExpression($2->type,$2,NULL,NULL); }
       | const {$$ = makeExpression($1->type,$1->sym,NULL,NULL); }
       ;

lvalue: IDENTIFIER		{
							if(libFuncCheck($1)){ 
								SymbolTableEntry_t *res = upStreamLookUp(currScope, $1);
								if(res != NULL){
									if(res->type == libfunc )yyerror("Redifinition of token");	
									else if(res->type != global){
										if(res->unionType == unionVar){ 
											if(res->value.varVal->scope != currScope){
												yyerror("Not accesible variable");//TODO: Check for loops

											}
										}
									}
									if(res->gramType == gr_conststring) $$ = makeExpression(conststring_e,res,NULL,NULL);
									else if(res->gramType == gr_string) $$ = makeExpression(conststring_e,res,NULL,NULL);
									else if(res->gramType == gr_boolean) $$ = makeExpression(constbool_e,res,NULL,NULL);
									else if(res->gramType == gr_nil) $$ = makeExpression(nil_e,res,NULL,NULL);
									else if(res->gramType == gr_integer) $$ = makeExpression(constnum_e,res,NULL,NULL);
									else if(res->gramType == gr_constinteger) $$ = makeExpression(constnum_e,res,NULL,NULL);
									else if(res->gramType == gr_real) $$ = makeExpression(constnum_e,res,NULL,NULL);
									else if(res->gramType == gr_constreal) $$ = makeExpression(constnum_e,res,NULL,NULL);
									else if(res->gramType == gr_funcaddr) $$ = makeExpression(programfunc_e,res,NULL,NULL);
								}
								else{ (currScope == 0) ? (res = makeVariableEntry($1,global)) : (res = makeVariableEntry($1,local));
									$$ = makeExpression(var_e,res,NULL,NULL);
									
									
								}
							}else $$ = makeExpression(libraryfunc_e, upStreamLookUp(currScope, $1), NULL, NULL);
						}
						
     
	  | LOCAL IDENTIFIER	{ 	
	  				if(libFuncCheck($2)){
						SymbolTableEntry_t* res = scopeLookUp(currScope, $2);
						if(res == NULL){
							res = makeVariableEntry($2,local);
							$$ = makeExpression(var_e,res,NULL,NULL);
						}else{
							
							if(res->gramType == gr_conststring) $$ = makeExpression(conststring_e,res,NULL,NULL);
							else if(res->gramType == gr_string) $$ = makeExpression(conststring_e,res,NULL,NULL);
							else if(res->gramType == gr_boolean) $$ = makeExpression(constbool_e,res,NULL,NULL);
							else if(res->gramType == gr_nil) $$ = makeExpression(nil_e,res,NULL,NULL);
							else if(res->gramType == gr_integer) $$ = makeExpression(constnum_e,res,NULL,NULL);
							else if(res->gramType == gr_constinteger) $$ = makeExpression(constnum_e,res,NULL,NULL);
							else if(res->gramType == gr_real) $$ = makeExpression(constnum_e,res,NULL,NULL);
							else if(res->gramType == gr_constreal) $$ = makeExpression(constnum_e,res,NULL,NULL);
							else if(res->gramType == gr_funcaddr) $$ = makeExpression(programfunc_e,res,NULL,NULL);
						}
					} else {
						$$ = makeExpression(libraryfunc_e, scopeLookUp(currScope, $1), NULL, NULL);
					       }
				}     
      | DOUBLECOLON IDENTIFIER	{ 
      				  SymbolTableEntry_t* res = scopeLookUp(0,$2); 	
      				  if(res == NULL){ 
      					 yyerror("Global Variable not found");
					 $$ = makeExpression(nil_e, NULL, NULL, NULL);
				  }else{
					if(res->gramType == gr_conststring) $$ = makeExpression(conststring_e,res,NULL,NULL);
					else if(res->gramType == gr_string) $$ = makeExpression(conststring_e,res,NULL,NULL);
					else if(res->gramType == gr_boolean) $$ = makeExpression(constbool_e,res,NULL,NULL);
					else if(res->gramType == gr_nil) $$ = makeExpression(nil_e,res,NULL,NULL);
					else if(res->gramType == gr_integer) $$ = makeExpression(constnum_e,res,NULL,NULL);
					else if(res->gramType == gr_constinteger) $$ = makeExpression(constnum_e,res,NULL,NULL);
					else if(res->gramType == gr_real) $$ = makeExpression(constnum_e,res,NULL,NULL);
					else if(res->gramType == gr_constreal) $$ = makeExpression(constnum_e,res,NULL,NULL);
					else if(res->gramType == gr_funcaddr) $$ = makeExpression(programfunc_e,res,NULL,NULL);
				  }
				}
      | member {$$ = $1;}
      ;

call_lvalue: IDENTIFIER				{SymbolTableEntry_t *res = upStreamLookUp(currScope,$1);
								 if(res != NULL) {
								 	if((res->type != userfunc && res->type != libfunc) && (res->gramType != gr_funcaddr)) yyerror("Function not found");
								 	else {
										printf("calling function %s\n",res->grammarVal.funcPtr->symbol->name);
										if(res->type == userfunc){
											 $$ = makeExpression(programfunc_e,res,NULL,NULL);
								 		}else{
								 			 $$ = makeExpression(libraryfunc_e,res,NULL,NULL);
								 		}
									}
								 }else {
								 $$ = makeExpression(nil_e,NULL,NULL,NULL);
								 yyerror("Function not found");
								 
								 }
								
								 
								 
						}		
	   | DOUBLECOLON IDENTIFIER	{SymbolTableEntry_t *res = scopeLookUp(0,$2);
								 if(res != NULL){
									if((res->type != userfunc && res->type != libfunc) && (res->gramType != gr_funcaddr)) yyerror("Function not found");
									else {
										printf("caling function %s\n", res->value.varVal->name);
										if(res->type == userfunc){
											 $$ = makeExpression(programfunc_e,res,NULL,NULL);
								 		}else{
								 			 $$ = makeExpression(libraryfunc_e,res,NULL,NULL);
								 		}
									}
								}else {
									yyerror("Function not found");
									$$ = makeExpression(nil_e,NULL,NULL,NULL);
								}
					}
								
	   | member {$$ = makeExpression($1->type,$1->sym,0,0);}
	   ;

member: lvalue DOT IDENTIFIER {$$ = member_item($1,$3);}
      | lvalue SQBRACKETOPEN expr SQBRACKETCLOSE {
      						  $1 = emit_iftableitem($1);
      						  $$ = makeExpression(tableitem_e,$1->sym,$3,NULL);
						 }
      | call DOT IDENTIFIER
      | call SQBRACKETOPEN expr SQBRACKETCLOSE
      ;

call: call PARENTHOPEN elist PARENTHCLOSE {
    					   $$ = makeCall($$,$3);
					   elistFlag = 0;
					  }
    | call_lvalue callsuffix {
    			  $1 = emit_iftableitem($1);
			  if($2->method){
			  	
			 	struct exprNode* new = malloc(sizeof(struct exprNode));
				new->node = $1;
				new->next = $2->elist;
				$2->elist = new;

				
				$1 = emit_iftableitem(member_item($1,$2->name));
			  } 
			 
			  $$ = makeCall($1,$2->elist);
			  elistFlag = 0;
    			 }
    | lvalue methodcall {$1 = emit_iftableitem($1); 
    				printf("RAAAAAAAA\n");
			  	if($2->method){
			  	
			 	struct exprNode* new = malloc(sizeof(struct exprNode));
				new->node = $1;
				new->next = $2->elist;
				$2->elist = new;
				
				
				$1 = emit_iftableitem(member_item($1,$2->name));
			  } 
			 
			  $$ = makeCall($1,$2->elist);
			  elistFlag = 0;

			  }
    		
    | PARENTHOPEN funcdef PARENTHCLOSE PARENTHOPEN elist PARENTHCLOSE {
    								      	struct expr* func = makeExpression(programfunc_e,$2,NULL,NULL);
									$$ = makeCall(func,$5);
									elistFlag = 0;
								      }
    ;

callsuffix: normcall { $$ = $1; }
	  | methodcall { $$ = $1; }
	  ;

normcall: PARENTHOPEN elist PARENTHCLOSE {elistFlag = 0;$$ = malloc(sizeof(struct call)); $$->elist = $2; $$->method  = 0; $$->name = NULL; }
	;

methodcall: DOUBLEDOT IDENTIFIER PARENTHOPEN elist PARENTHCLOSE {elistFlag = 0; $$ = malloc(sizeof(struct call)); $$->elist = $4; $$->method  = 1; $$->name = strdup($2); }
	  ;

elist: expr	 {
				if(elistFlag == 0){
					elistFlag = 1;
					$$ = NULL;
				}

				if($$ == NULL){
					$$ = malloc(sizeof(struct exprNode));
					$$->node = $1;
					$$->next = NULL;
				}else{
					struct exprNode* newnode = malloc(sizeof(struct exprNode));
					newnode->node = $1;
					newnode->next = NULL;
					struct exprNode* temp = $$;
					
					while(temp->next != NULL){
						temp = temp->next;
					}
					temp->next = newnode;
				}
				
			 } 
     | elist COMMA expr  {
     				if(elistFlag == 0){
					elistFlag = 1;
					$$ = NULL;
				}

				if($$ == NULL){
					$$ = malloc(sizeof(struct exprNode));
					$$->node = $3;
					$$->next = NULL;

				}else{
					struct exprNode* newnode = malloc(sizeof(struct exprNode));
					newnode->node = $3;
					newnode->next = NULL;
					struct exprNode* temp = $$;
					
					while(temp->next != NULL){
						temp = temp->next;
					}
					temp->next = newnode;
				}


			 }
     | {$$ = NULL;}
     ;

indexed: indexedelem   {
				if(indexedFlag == 0){
					indexedFlag = 1;
					$$ = NULL;
				}

				if($$ == NULL){
					$$ = malloc(sizeof(struct indexed_elem));
					$$->index = $1->index;
					$$->value = $1->value;
					$$->next = NULL;
				}else{
					struct indexed_elem* newnode = malloc(sizeof(struct indexed_elem));
					newnode->index = $1->index;
					newnode->value = $1->value;
					newnode->next = NULL;
					struct indexed_elem* temp = $$;
					
					while(temp->next != NULL){
						temp = temp->next;
					}
					temp->next = newnode;
				}			
				
			}
        | indexed COMMA indexedelem {
        				if(indexedFlag == 0){
						indexedFlag = 1;
						$$ = NULL;
					}

					if($$ == NULL){
						$$ = malloc(sizeof(struct indexed_elem));
						$$->index = $3->index;
						$$->value = $3->value;
						$$->next = NULL;
					}else{
						struct indexed_elem* newnode = malloc(sizeof(struct indexed_elem));
						newnode->index = $3->index;
						newnode->value = $3->value;
						newnode->next = NULL;
						struct indexed_elem* temp = $$;
						
						while(temp->next != NULL){
							temp = temp->next;
						}
						temp->next = newnode;
					}			
        			     }
        ;


objectdef: SQBRACKETOPEN elist SQBRACKETCLOSE {
								struct expr* t = makeExpression(newtable_e,newtemp(),NULL,NULL); 
								emit(TABLECREATE,NULL,NULL,t,0,0);
								
								struct exprNode* head = $2;
								int i = 0;
								while (head != NULL){
									emit(TABLESETELEM,newexpr_constnum(i++),head->node,t,0,0);

									head = head->next;
								}								


								$$ = t;
					      }
	 ; 

objectdef: SQBRACKETOPEN indexed SQBRACKETCLOSE { 		
								struct expr* t = makeExpression(newtable_e,newtemp(),NULL,NULL); 
								emit(TABLECREATE,NULL,NULL,t,0,0); 
								
								struct indexed_elem* head = $2;
								while(head != NULL){
									emit(TABLESETELEM, head->index, head->value, t, 0, 0);
									
									head = head->next;
								}
								
								
								$$ = t;
								
						};



indexedelem: CURBRACKETOPEN expr COLON expr CURBRACKETCLOSE { 
								$$ = malloc(sizeof(struct indexed_elem));
								$$->index = $2;
								$$->value = $4;	
								
								
								};

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

funcblockstart:	{push_loopcounter(); infunction++;}

funcblockend:	{pop_loopcounter(); --infunction;}

funcname: IDENTIFIER	    {	
				if(!libFuncCheck($1)) yyerror("existing library function with same name"); 
				$$ = $1;
			     }

	|		    {	
				int stringLength = snprintf(NULL,0,"%d",anonymusFuncNum);
       				char* functionName = (char*)malloc(strlen("_anonymusfunc") + stringLength + 1);
				snprintf(functionName,stringLength + 1 + strlen("_anonymusfunc"),"_anonymusfunc%d",anonymusFuncNum);
				anonymusFuncNum++;	
				$$ = functionName;   
			}
	;

funcprefix: FUNCTION funcname	{	SymbolTableEntry_t* newFunc = makeFuncEntry($2, userfunc);
	  				newFunc->grammarVal.funcPtr = newFunc;
					$$ = newFunc;
	  				struct expr* newExpr = makeExpression(programfunc_e, newFunc, NULL, NULL);
					$$->value.funcVal->qaddress = nextquadlabel();
					emit(FUNCSTART, NULL, NULL, newExpr, 0,0);
					//push ???? TODO
					enterscopespace();
					resetformalargsoffset();
	  			}
	  ;


funcargs: PARENTHOPEN{currScope++; allocateScopes(currScope);} idlist {currScope--;}PARENTHCLOSE {$$ = makeFuncArgList(NULL,currScope); enterscopespace(); resetfunctionlocalsoffset();}
	;

funcbody: block {$$ = currscopeoffset(); exitscopespace();}
	;

funcdef: funcprefix funcargs funcblockstart funcbody funcblockend { 
       					exitscopespace();
       					$1->value.funcVal->totallocals = $4;
					printf("Number of local variables %u\n",$4);
					//TODO: int offset = popandtop
					//restorecurrscopeoffset(offset);
					$1->value.funcVal->arglist = $2;
					$$ = $1;
					struct expr* newExpr = makeExpression(programfunc_e, $1, NULL, NULL);
					emit(FUNCEND, NULL, NULL, newExpr, 0, 0);
				      }
       					
       ;

const: number {$$ = malloc(sizeof(struct expr)); $$->sym = $1; $$->type = constnum_e; $$->index = NULL; $$->next = NULL;  } 
     | STRING {$$ = malloc(sizeof(struct expr)); $$->type = conststring_e;$$->sym = malloc(sizeof(SymbolTableEntry_t));$$->sym->symbol = malloc(sizeof(struct sym)); 
     		$$->sym->gramType = gr_conststring; 
     		$$->sym->symbol->name = "const_string"; $$->sym->grammarVal.string = malloc(strlen($1)+1); 
     		strcpy($$->sym->grammarVal.string, $1); $$->type = conststring_e; $$->index = NULL; $$->next = NULL;}
     | NIL {$$ = malloc(sizeof(struct expr)); $$->sym = malloc(sizeof(SymbolTableEntry_t));$$->sym->symbol = malloc(sizeof(struct sym)); $$->sym->gramType = gr_nil; 
     		$$->sym->symbol->name = "nil"; $$->sym->grammarVal.nil = 1; $$->type = nil_e; $$->index = NULL;
     		$$->next = NULL;}
     | TRUE {$$ = malloc(sizeof(struct expr)); $$->sym = malloc(sizeof(SymbolTableEntry_t)); $$->sym->symbol = malloc(sizeof(struct sym));$$->sym->gramType = gr_boolean; 
     		$$->sym->symbol->name = "const_true"; $$->sym->grammarVal.boolean = 1; $$->type = constbool_e; 
     		$$->index = NULL; $$->next = NULL;}
     | FALSE {$$ = malloc(sizeof(struct expr)); $$->sym = malloc(sizeof(SymbolTableEntry_t)); $$->sym->symbol = malloc(sizeof(struct sym));$$->sym->gramType = gr_boolean;
     		$$->sym->symbol->name = "const_false";$$->sym->grammarVal.boolean = 0; $$->type = constbool_e; 
     		$$->index = NULL;$$->next = NULL;}
     ;

number: INTEGER {$$ = malloc(sizeof(SymbolTableEntry_t));
                    int length = snprintf(NULL,0,"%d",$1);
                    $$->symbol = malloc(sizeof(struct sym));
                    $$->symbol->name = malloc(length + 1);
                    snprintf($$->symbol->name,length + 1,"%d",$1);
                    $$->gramType = gr_integer;
                    $$->grammarVal.intNum = $1;
					$$->value.varVal = malloc(sizeof(Variable_t));
					$$->value.varVal->line = yylineno;
                }
      | REAL{$$ = malloc(sizeof(SymbolTableEntry_t));
                                int length = snprintf(NULL,0,"%f",$1);
                                 $$->symbol = malloc(sizeof(struct sym));
                                 $$->symbol->name = malloc(length + 1);
                                 snprintf($$->symbol->name,length + 1,"%f",$1);
                                 $$->gramType = gr_real;
                                 $$->grammarVal.realNum = $1;
								 $$->value.varVal = malloc(sizeof(Variable_t));
					             $$->value.varVal->line = yylineno;
					}
      ;


idlist: IDENTIFIER {SymbolTableEntry_t* res = scopeLookUp(currScope,$1);
      		    if(res != NULL){
		    	yyerror("Same formal argument given ");
		    }
		    makeVariableEntry($1,formal);}
      | idlist COMMA IDENTIFIER {SymbolTableEntry_t* res = scopeLookUp(currScope,$3);
      				 if(res != NULL){
				 	yyerror("Same formal argument given ");
				 }
				 makeVariableEntry($3,formal);}
      |
      ;

ifprefix: IF PARENTHOPEN expr PARENTHCLOSE{
												emit(IF_EQ, $3, newexpr_constbool(1), NULL, nextquadlabel() + 2, 0);

												$$ = nextquadlabel();

												emit(JUMP, NULL, NULL, NULL, 0, 0);
											}
elseprefix: ELSE{
						$$ = nextquadlabel();
						emit(JUMP, NULL, NULL, NULL, 0, 0);
				}


if: ifprefix stmts {
						patchlabel($1, nextquadlabel());
					}

	| ifprefix stmts elseprefix stmts{
										patchlabel($1, $3+1);
										patchlabel($3, nextquadlabel());
																
									}
	;



loopstart:	{++loopcounter;}

loopend:	{--loopcounter;}

loopstmt: loopstart stmts loopend	{$$ = $2;}

whilestart: WHILE {
		    $$ = nextquadlabel();  
		  }

whilecond: PARENTHOPEN {currScope++; allocateScopes(currScope); } expr PARENTHCLOSE  {
	 				   currScope--;
					   emit(IF_EQ,$3,newexpr_constbool(1),NULL,nextquadlabel() + 2,0);
					   $$ = nextquadlabel();
					   emit(JUMP,NULL,NULL,NULL,0,0);
	 				 }

while: whilestart whilecond loopstmt {
     					emit(JUMP,NULL,NULL,NULL,$1,0);
					patchlabel($2,nextquadlabel());
					patchlist($3->breakList,nextquadlabel());
					patchlist($3->contList,$1);

       				      }

unfinjmp:{ $$ = nextquadlabel(); emit(JUMP,NULL,NULL,NULL,0,0); }

forretlabel: { $$ = nextquadlabel(); }


forprefix: FOR PARENTHOPEN {currScope++; allocateScopes(currScope); } elist {elistFlag = 0;} SEMICOLON forretlabel expr SEMICOLON {
	 							       $$ = malloc(sizeof(struct for_labels));
	 							       $$->test = $7;
								       $$->enter = nextquadlabel();
								       emit(IF_EQ,$8,newexpr_constbool(1),NULL,0,0);
								      }
forpostfix: forprefix unfinjmp elist PARENTHCLOSE {currScope--; elistFlag = 0;} unfinjmp loopstmt unfinjmp {
   								    	   patchlabel($1->enter,$6 + 1);
								    	   patchlabel($2,nextquadlabel());
								    	   patchlabel($6,$1->test);
								    	   patchlabel($8,$2 + 1);

								    	   patchlist($7->breakList,nextquadlabel());
								    	   patchlist($7->contList,$2 + 1);
   								  	 }

for: forpostfix 


/*forstmt: FOR PARENTHOPEN {currscope++; allocatescopes(currscope); } elist SEMICOLON expr SEMICOLON elist PARENTHCLOSE {currScope--;} stmt  ;*/



returnstmt: RETURN SEMICOLON { if(infunction > 0){
	  		      	emit(RET,NULL,NULL,NULL,0,0);
			      }else{
			      	yyerror("Return outside of function");
			      }
			     }
	  | RETURN expr SEMICOLON { if(infunction > 0){
	  			    emit(RET,$2,NULL,NULL,0,0);
	  			   }else{
				   	yyerror("Return outside of function");
				   }
				  }
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

	push_loopcounter();

	yyparse();

	printScopeLists();
	printQuads();

    	return 0;
}

/*PHASE 3*/

enum scope_space currscopespace(void){
	if(scopeSpaceCounter == 1){
		return program_var;
	}else if(scopeSpaceCounter % 2 == 0){
		return formal_arg;
	}else{
		return function_loc;
	}
}

unsigned currscopeoffset(void){
	switch(currscopespace()){
		case program_var	: return programVarOffset;
		case function_loc : return functionLocalOffset;
		case formal_arg : return formalArgOffset;
		default: assert(0);
	}
}

void inccurrscopeoffset(void){
	switch(currscopespace()){
		case program_var : ++programVarOffset; break;
		case function_loc : ++functionLocalOffset; break;
		case formal_arg : ++formalArgOffset; break;
		default : assert(0);
	}
}

void enterscopespace(void) { ++scopeSpaceCounter; }

void exitscopespace(void) { assert(scopeSpaceCounter > 1); --scopeSpaceCounter; }

void resetformalargsoffset(void) { formalArgOffset = 0; }

void resetfunctionlocalsoffset(void) { functionLocalOffset = 0; }

void restorecurrscopeoffset(unsigned n) { 
	switch(currscopespace()){
		case program_var : programVarOffset = n; break;
		case function_loc : functionLocalOffset = n; break;
		case formal_arg : formalArgOffset = n; break;
		default : assert(0);
	}
}

unsigned nextquadlabel(void){ return currQuad; }

void patchlabel(unsigned quadNo, unsigned label){
	assert(quadNo < currQuad);
	quads[quadNo].label = label;
}

char* newtempname(){
	int stringLength = snprintf(NULL,0,"%d",tempcounter);
	char* tempname = (char*)malloc(strlen("_temp") + stringLength + 1);
	snprintf(tempname,stringLength + 1 + strlen("_temp"),"_temp%d",tempcounter);
	return tempname;
}

void resettemp(){
	tempcounter = 0;
}

SymbolTableEntry_t* newtemp(){
	char* name = newtempname();
	SymbolTableEntry_t* sym = upStreamLookUp(currScope,name);
	if(sym == NULL){
		tempcounter++;
		return makeVariableEntry(name,temp); 
	}else{
		return sym;
	}
}
	

void expand(){
	assert(total == currQuad);
	struct quad* p = malloc(NEW_SIZE);
	if(quads){
		memcpy(p,quads,CURR_SIZE);
		free(quads);
	}
	quads = p;
	total += EXPAND_SIZE;
}

void emit(
	enum iopcode op,
	struct expr* arg1,
	struct expr* arg2,
	struct expr* result,
	unsigned label,
	unsigned line
	) {

	if(currQuad == total){
		expand();
	}

	struct quad* p = quads + currQuad++;
	p->op = op;
	p->arg1 = arg1;
	p->arg2 = arg2;
	p->result = result;
	p->label = label;
	p->line = line;
}

struct exprNode* reverseList(struct exprNode* head) {
     struct exprNode*  *prev = NULL, *current = head, *next = NULL;
        while (current != NULL) {
	        next = current->next;
		current->next = prev;
		prev = current;
		current = next;
	}
	return prev;
}


struct expr* makeCall(struct expr* lv,struct exprNode* head){
	struct expr* func = emit_iftableitem(lv);
	//FunctArgNode_t* head = func->sym->value.funcVal->arglist;
	struct exprNode* reversed = NULL;
	
	reversed = reverseList(head);
	while(reversed != NULL){
		//struct expr* temp = makeExpression(var_e,reversed->arg,NULL,NULL);
		emit(PARAM,reversed->node,NULL,NULL,0,0);
		reversed = reversed->next;
	}
	emit(CALL,func,NULL,NULL,0,0);
	struct expr* result = makeExpression(var_e,newtemp(),NULL,NULL);
	emit(GETRETVAL,NULL,NULL,result,0,0);
	return result;
}

void printQuads(void){
	FILE* file;
	file = fopen("quads.txt","w");
	
	if(file == NULL){
		yyerror("Couldnt make quads.txt file");
	}

	fprintf(file, "%-8s %-20s %-20s %-20s %-20s %-20s\n", "Quad#", "opcode", "result", "arg1", "arg2", "label");
	fprintf(file,"-------------------------------------------------------------------------------------------------------------\n");
	
	for(int i =0; i < currQuad;i++){
		

		fprintf(file,"%-8d",i);
		/*:))))))))))*/
		switch (quads[i].op){
				case ASSIGN:
					fprintf(file,"%-20s ", "assign");break;
				case ADD:
					fprintf(file,"%-20s ","add");break;
				case SUB:
					fprintf(file,"%-20s ","sub");break;
				case MUL:
					fprintf(file,"%-20s ","mul");break;
				case DIV:
					fprintf(file,"%-20s ","div");break;
				case MOD:
					fprintf(file,"%-20s ","mod");break;
				case UMINUS:
					fprintf(file,"%-20s ","uminus");break;
				case OP_AND:
					fprintf(file,"%-20s ","and");break;
				case OP_OR:
					fprintf(file,"%-20s ","or");break;
				case OP_NOT:
					fprintf(file,"%-20s ","not");break;
				case IF_EQ:
					fprintf(file,"%-20s ","if_eq");break;
				case IF_NOTEQ:
					fprintf(file,"%-20s ","if_noteq");break;
				case IF_LESSEQ:
					fprintf(file,"%-20s ","if_lesseq ");break;
				case IF_GREATEREQ:
					fprintf(file,"%-20s ","if_greatereq");break;
				case IF_LESS:
					fprintf(file,"%-20s ","if_less");break;
				case IF_GREATER:
					fprintf(file,"%-20s ","if_greater");break;
				case CALL:
					fprintf(file,"%-20s ","call");break;
				case PARAM:
					fprintf(file,"%-20s ","param");break;
				case RET:
					fprintf(file,"%-20s ","ret");break;
				case GETRETVAL:
					fprintf(file,"%-20s ","getretval");break;
				case FUNCSTART:
					fprintf(file,"%-20s ","funcstart");break;
				case FUNCEND:
					fprintf(file,"%-20s ","funcend");break;
				case TABLECREATE:
					fprintf(file,"%-20s ","tablecreate");break;
				case TABLEGETELEM:
					fprintf(file,"%-20s ","tablegetelem");break;
				case TABLESETELEM:
					fprintf(file,"%-20s ","tablesetelem");break;
				case JUMP:
					fprintf(file,"%-20s ","jump");break;
				default:
					fprintf(file,"%-20s ","unknownopcode");break;
			}


		
		if(quads[i].result == NULL) fprintf(file,"%-20s", "-");
		else fprintf(file, "%-20s", quads[i].result->sym->symbol->name);

		if(quads[i].arg1 == NULL) fprintf(file, "%-20s", "-");
		else fprintf(file, "%-20s", quads[i].arg1->sym->symbol->name);

		if(quads[i].arg2 == NULL) fprintf(file, "%-20s", "-");
		else fprintf(file, "%-20s", quads[i].arg2->sym->symbol->name); 
		
		if(quads[i].label == 0){
			fprintf(file,"%-20s\n", "-");
		}else{
			fprintf(file,"%d\n",quads[i].label);
		}
	}
	
	fclose(file);

}

struct expr* makeExpression(enum expr_en type, SymbolTableEntry_t* sym, struct expr* index, struct expr* next){
	
	struct expr* newExpr = malloc(sizeof(struct expr));
	newExpr->type = type;
	newExpr->sym = sym;
	newExpr->index = index;
	newExpr->next = next;

	return newExpr;

}

struct expr* newexpr_constnum(int value){
	int length = snprintf(NULL,0,"%d",value);
	SymbolTableEntry_t* newentry = malloc(sizeof(SymbolTableEntry_t));
	newentry->symbol = malloc(sizeof(struct sym));
	newentry->symbol->name = malloc(length + 1);
	snprintf(newentry->symbol->name,length + 1,"%d",value);
	newentry->grammarVal.intNum = value;
	newentry->gramType = gr_constinteger;
	struct expr* newexpr = makeExpression(constnum_e,newentry,NULL,NULL);
	return newexpr;
}

unsigned int istempname(char* s){
	return *s == '_';
}

unsigned int istempexpr(struct expr* e){
	return e->sym->symbol->name && istempname(e->sym->symbol->name);
}


struct expr* newexpr_constbool(short int value){
	SymbolTableEntry_t* newentry = malloc(sizeof(SymbolTableEntry_t));
	newentry->symbol = malloc(sizeof(struct sym));
	if(value == 0){
		newentry->symbol->name = "\'false\'";
	}else{
		newentry->symbol->name = "\'true\'";
	}
	newentry->grammarVal.boolean = value;
	newentry->gramType = gr_boolean;
	struct expr* newexpr = makeExpression(constbool_e,newentry,NULL,NULL);
	return newexpr;
}


void push_loopcounter(void){
	struct lc_stack_t* new = malloc(sizeof(struct lc_stack_t));
	new->counter = 0;
	new->next = NULL;
	if(lcs_top == 0){
		lcs_top = new;
		lcs_bottom = new;
	}else{
		lcs_top->next = new;
		lcs_top = new;
	}
}

void pop_loopcounter(void){
	if(lcs_top == 0){
		return;
	}

	if(lcs_top == lcs_bottom){
		//free(lcs_top);
		lcs_top = 0;
		lcs_bottom = 0;
		return;
	}
	
	struct lc_stack_t* temp = lcs_bottom;
	struct lc_stack_t* prev = NULL;
	
	while(temp->next != NULL){
		prev = temp;
		temp = temp->next;
	}
	
	lcs_top = prev;
	//free(temp);
	return;
}

struct stmt_t* make_stmt(){
	struct stmt_t* s = malloc(sizeof(struct stmt_t));
	s->breakList = s->contList = 0;
	return s;
}

int newlist(int i){
	quads[i].label = 0;
	return i;
}

int mergelist(int l1,int l2){
	if(!l1){
		return l2;
	}else if(!l2){
		return l1;
	}else{
		int i = l1;
		while(quads[i].label){
			i = quads[i].label;
		}
		quads[i].label = l2;
		return l1;
	}
}

void patchlist(int list,unsigned label){
	while(list){
		int next = quads[list].label;
		quads[list].label = label;
		list = next;
	}

}

void printStack(int list){
	while(list){
		int next = quads[list].label;
		printf("%u label\n",next);
		list = next;
	}
}



/*end of phase3*/

SymbolTableEntry_t* makeLibEntry(char *name){

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
	entry->gramType = gr_funcaddr;
	
	entry->symbol = malloc(sizeof(struct sym));

	assert(entry->symbol);
	
	entry->symbol->name = name;
	entry->symbol->type = libraryfunc_s;
	entry->symbol->scopeSpace = currscopespace();
	entry->symbol->offset = currscopeoffset();
	inccurrscopeoffset();
	entry->grammarVal.funcPtr = entry;

    	insertToSymTable(currScope, f->name, entry);

	return entry;

}

SymbolTableEntry_t* makeVariableEntry(char *name, enum SymbolType type){
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

	entry->symbol = malloc(sizeof(struct sym));
	
	assert(entry->symbol);
	
	entry->symbol->name = name;
	entry->symbol->type = var_s;
	entry->symbol->scopeSpace = currscopespace();
	entry->symbol->offset = currscopeoffset();
	inccurrscopeoffset();

    	insertToSymTable(currScope, v->name, entry);
	return entry;
}
SymbolTableEntry_t* makeFuncEntry(char *name,enum SymbolType type){
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
	entry->gramType = gr_funcaddr;

	entry->symbol = malloc(sizeof(struct sym));
	
	assert(entry->symbol);

	entry->symbol->name = name;
	entry->symbol->type = programfunc_s;
	entry->symbol->scopeSpace = currscopespace();
	entry->symbol->offset = currscopeoffset();
	inccurrscopeoffset();

	insertToSymTable(currScope,f->name,entry);
	printFuncArgs(f->arglist);
	return entry;
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
				case temp:
					printf("[temp variable] ");
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

}

/*phase 3*/
struct expr* emit_iftableitem(struct expr* e){
	if(e->type != tableitem_e)
		return e;
	else {
		struct expr* result = makeExpression(var_e,newtemp(),e->index,NULL);
		emit(TABLEGETELEM, e, e->index, result,0,0);
		return result;
	}
}

struct expr* member_item(struct expr* lvalue, char* name){
	lvalue = emit_iftableitem(lvalue);
	struct expr* item = makeExpression(tableitem_e,lvalue->sym,NULL,NULL);
	item->index = newexpr_conststring(name);
	return item;
}

struct expr* newexpr_conststring(char* name){
	SymbolTableEntry_t* newentry = malloc(sizeof(SymbolTableEntry_t));
	newentry->symbol = malloc(sizeof(struct sym));
	newentry->symbol->name = strdup(name);
	newentry->grammarVal.string = strdup(name);
	newentry->gramType = gr_conststring;
	struct expr* newexpr = makeExpression(conststring_e,newentry,NULL,NULL);
	return newexpr;
}



void checkArithmetic(struct expr* e){
	if(e->type == constbool_e ||
	   e->type == conststring_e ||
	   e->type == nil_e ||
	   e->type == newtable_e ||
	   e->type == programfunc_e ||
	   e->type == libraryfunc_e ||
	   e->type == boolexpr_e){
	   	yyerror("Illegal expression ");
	   }
}
