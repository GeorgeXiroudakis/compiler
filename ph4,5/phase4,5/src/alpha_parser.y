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
unsigned int currQuad = 1;

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

struct scopeoffsetstack* sos_top = 0;
struct scopeoffsetstack* sos_bottom = 0;

int infunction = 0;

#define loopcounter (lcs_top->counter)

void expand();

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
void restorecurrscopeoffset(unsigned n);
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
void push_scopeoffset(unsigned currscopeoff);
int  popandtop(void);


/* PHASE 4 */

double*			numConsts;
unsigned		totalNumConsts;
char**			stringConsts;
unsigned		totalStringConsts;
char**			nameLibfuncs;
unsigned		totalNamedLibfuncs;
struct userfunc*	userFuncs;
unsigned		totalUserFuncs;


struct incomplete_jump* ij_head = (struct incomplete_jump*)0;
unsigned		ij_total = 0;

struct instruction* instructions = (struct instruction*)0;

unsigned tcodeSize = 0;
unsigned totalInstr = 0;
unsigned int currInstr = 1;

unsigned processedQuads = 0; 

char** strArray = NULL;
unsigned strCounter = 0;

double* numArray = NULL;
unsigned numCounter = 0;

Function_t** funcArray = NULL;
unsigned funcCounter = 0;

char** libfuncArray = NULL;
unsigned libfuncCounter = 0;

struct func_stack* head = NULL;

void expand_instructions();

unsigned consts_newstring(char* s);
unsigned consts_newnumber(double n);
unsigned libfuncs_newused(char* s);
unsigned userfuncs_newfunc(SymbolTableEntry_t* s);
void make_operand(struct expr* e,struct vmarg* arg);
void make_numberoperand(struct vmarg* arg, double val);
void make_booloperand(struct  vmarg* arg, unsigned val);
void make_revaloperand(struct  vmarg* arg);
void add_incomplete_jump(unsigned int instrNo,unsigned iaddress);
void patch_incomplete_jumps();
void generate(enum vmopcode op,struct quad* q);
unsigned nextinstructionlabel(void);
void emit_instruction(struct instruction* i);
unsigned currprocessedquad();
void append_retList(Function_t* ret ,unsigned label);
void backpatch_retlist(struct return_list* ret,unsigned label);
void printInstructions();
void printEnum(FILE* file,struct vmarg* arg);
void printValArray();

void generate_ADD (struct quad* q);
void generate_SUB (struct quad* q);
void generate_MUL (struct quad* q);
void generate_DIV (struct quad* q);
void generate_MOD (struct quad* q); 
void generate_NEWTABLE (struct quad* q);   
void generate_TABLEGETELEM (struct quad* q);  
void generate_TABLESETELEM (struct quad* q);
void generate_ASSIGN (struct quad* q);   
void generate_NOP (struct quad* q);   
void generate_JUMP (struct quad* q);  
void generate_IF_EQ (struct quad* q);   
void generate_IF_NOTEQ (struct quad* q);  
void generate_IF_GREATER (struct quad* q);   
void generate_IF_GREATEREQ (struct quad* q);  
void generate_IF_LESS (struct quad* q);   
void generate_IF_LESSEQ (struct quad* q);
void generate_NOT (struct quad* q);
void generate_OR (struct quad* q);
void generate_PARAM (struct quad* q);  
void generate_CALL (struct quad* q); 
void generate_GETRETVAL (struct quad* q);
void generate_FUNCSTART (struct quad* q);
void generate_FUNCEND (struct quad* q);
void generate_RETURN (struct quad* q);
void execute_jmp(struct instruction* i);
void generate_UMINUS(struct quad* q);

void push_funcstack(Function_t* f);
Function_t* pop_funcstack(void);
Function_t* top_funcstack(void);

typedef void (*generator_func_t)(struct quad*);

generator_func_t generators[] = {
	generate_ASSIGN,
	generate_ADD,
	generate_SUB,
	generate_MUL,
	generate_DIV,
	generate_MOD,
	generate_UMINUS,
	generate_IF_EQ,
	generate_IF_NOTEQ,
	generate_IF_LESSEQ,
	generate_IF_GREATEREQ,
	generate_IF_LESS,
	generate_IF_GREATER,
	generate_CALL,
	generate_PARAM,
	generate_RETURN,
	generate_GETRETVAL,
	generate_FUNCSTART,
	generate_FUNCEND,
	generate_NEWTABLE,
	generate_TABLEGETELEM,
	generate_TABLESETELEM,
	generate_JUMP
};

void generate_instructions(void);

/* END OF PHASE 4 */

/* PHASE 5 */


typedef void (*memclear_func_t) (struct avm_memcell*);
typedef void (*execute_func_t) (struct instruction*);

struct avm_memcell ax, bx, cx;
struct avm_memcell retval;
unsigned top, topsp;
unsigned char executionFinished = 0;
unsigned pc = 1;
unsigned currLine = 0;
//unsigned codeSize = 0;
struct instruction* code = (struct instruction*) 0;
unsigned totalActuals = 0;


struct avm_memcell stack[AVM_STACKSIZE];

double consts_getnumber(unsigned index);
char* consts_getstring(unsigned index);
char* libfuncs_getused(unsigned index);
Function_t* userfuncs_getfunc(unsigned index);

static void avm_initstack(void);
struct avm_table* avm_tablenew(void);
void avm_tabledestroy(struct avm_table*);
struct avm_memcell* avm_tablegetelem(struct avm_table*, struct avm_memcell* key);
void avm_tablesetelem(struct avm_table* table,struct avm_memcell* key, struct avm_memcell* value);
void avm_tableincrefcounter(struct avm_table* t);
void avm_tabledecrefcounter(struct avm_table* t);
void avm_tablebucketsinit(struct avm_table_bucket** p);
void avm_tablebucketsdestroy(struct avm_table_bucket** p);
void avm_tabledestroy(struct avm_table*);
struct avm_memcell* avm_translate_operand(struct vmarg* arg,struct avm_memcell* reg);

void execute_assign(struct instruction*);
/*void execute_add(struct instruction*);
void execute_sub(struct instruction*);
void execute_mul(struct instruction*);
void execute_div(struct instruction*);
void execute_mod(struct instruction*);*/

void execute_uminus(struct instruction*);
void execute_and(struct instruction*);
void execute_or(struct instruction*);
void execute_not(struct instruction*);

void execute_jeq(struct instruction*);
void execute_jne(struct instruction*);
void execute_jle(struct instruction*);
void execute_jge(struct instruction*);
void execute_jlt(struct instruction*);
void execute_jgt(struct instruction*);
void execute_call(struct instruction*);
void execute_pusharg(struct instruction*);
void execute_funcenter(struct instruction*);
void execute_funcexit(struct instruction*);
void execute_newtable(struct instruction*);
void execute_tablegetelem(struct instruction*);
void execute_tablesetelem(struct instruction*);
void execute_nop(struct instruction*);

execute_func_t executeFuncs[] = {
	execute_assign,
	execute_add,
	execute_sub,
	execute_mul,
	execute_div,
	execute_mod,
	execute_uminus,
	execute_and,
	execute_or,
	execute_not,
	execute_jeq,
	execute_jne,
	execute_jle,
	execute_jge,
	execute_jlt,
	execute_jgt,
	execute_call,
	execute_pusharg,
	execute_funcenter,
	execute_funcexit,
	execute_newtable,
	execute_tablegetelem,
	execute_tablesetelem,
	execute_jmp,
	execute_nop
};

int execute_cycle(void);
void memclear_string(struct avm_memcell* m);
void memclear_table(struct avm_memcell* m);
void avm_memcellclear(struct avm_memcell* m);

memclear_func_t memclearFuncs[] = {
	0, /*number*/
	memclear_string,
	0, /*bool*/
	memclear_table,
	0, /*userfunc*/
	0, /*libfunc*/
	0, /*nil*/
	0  /*undef*/
};

void avm_warning(char* format,char* v);
void avm_assign(struct avm_memcell* lv,struct avm_memcell* rv);

void avm_error(char* format,char* v);
char* avm_tostring(struct avm_memcell* m); /*Caller frees.*/
unsigned char avm_tobool(struct avm_memcell* m);
void avm_calllibfunc(char* funcName);
void avm_callsaveenviroment(void);
void avm_call_functor(struct avm_table* t);
void avm_push_table_arg(struct avm_table* t); /*latter*/
void avm_dec_top(void);
void avm_push_envvalue(unsigned val);
Function_t* avm_getfuncinfo(unsigned address);
unsigned avm_get_envvalue(unsigned i);
unsigned avm_totalactuals(void);
struct avm_memcell* avm_getactual(unsigned i);
//void avm_registerlibfunc(char* id,library_func_t addr); TODO wat
void libfunc_print(void);
void avm_call_functor(struct avm_table* t);

typedef char* (*tostring_func_t) (struct avm_memcell*m);

char* number_tostring(struct avm_memcell* m);
char* string_tostring(struct avm_memcell* m);
char* bool_tostring(struct avm_memcell* m);
char* table_tostring(struct avm_memcell* m);
char* userfunc_tostring(struct avm_memcell* m);
char* libfunc_tostring(struct avm_memcell* m);
char* nil_tostring(struct avm_memcell* m);
char* undef_tostring(struct avm_memcell* m);

tostring_func_t tostringFuncs[] = {
	number_tostring,
	string_tostring,
	bool_tostring,
	table_tostring,
	userfunc_tostring,
	libfunc_tostring,
	nil_tostring,
	undef_tostring

};

char* avm_tostring(struct avm_memcell* m);

typedef double (*arithmetic_func_t) (double x,double y);

double add_impl(double x,double y);
double sub_impl(double x,double y);
double mul_impl(double x,double y);
double div_impl(double x,double y);
double mod_impl(double x,double y);

arithmetic_func_t arithmeticFuncs[] = {
	add_impl,
	sub_impl,
	mul_impl,
	div_impl,
	mod_impl
};

unsigned int avm_table_hash(const char *pcKey);

struct avm_table_bucket* getTableBucket(struct avm_table* table, struct avm_memcell* key);

void setTableBucket(struct avm_table* table, struct avm_table_bucket* bucket);

void run_alphaprogram(void);

/* END OF PHASE 5 */
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

%type<unsignedVal> funcbody ifprefix elseprefix whilestart whilecond unfinjmp forretlabel quadsave

%type<entryNode> funcdef funcprefix

%type<argNode> funcargs

%type<callType> methodcall normcall callsuffix

%type<indexedType> indexed indexedelem

%type<forLabelsType> forprefix 

%type<stmtType> stmt loopstmt break continue stmts while for if returnstmt block forpostfix





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

stmt: expr SEMICOLON {$$ = make_stmt();}
    | if {$$ = make_stmt(); $$->breakList = $1->breakList; $$->contList = $1->contList; }
    | while {$$ = make_stmt();}
    | for {$$ = make_stmt();}
    | returnstmt {$$ = make_stmt(); $$->breakList = $1->breakList; $$->contList = $1->contList; }
    | break {$$ = make_stmt(); $$->breakList = $1->breakList; $$->contList = $1->contList; }
    | continue {$$ = make_stmt(); $$->breakList = $1->breakList; $$->contList = $1->contList; }
    | block { $$ = make_stmt(); $$->breakList = $1->breakList; $$->contList = $1->contList; }
    | funcdef { $$ = make_stmt(); }
    | SEMICOLON { $$ = make_stmt(); }
    ;
    
    break: BREAK SEMICOLON{	$$ = make_stmt();
    				if(loopcounter != 0){	
	    				$$->breakList = newlist(nextquadlabel());
	    				emit(JUMP, NULL, NULL, NULL, 0, 0);
	    			} else {
	    				yyerror("Illegal break: not in a loop");
	    			}
    			}
    
    continue: CONTINUE SEMICOLON{    				
	    				$$ = make_stmt();
	    				if(loopcounter != 0){	
		    				$$->contList = newlist(nextquadlabel());
    						emit(JUMP, NULL, NULL, NULL, 0, 0);
		    			} else {
		    				yyerror("Illegal continue: not in a loop");
		    			}
    				}

stmts: stmts stmt{
			$$ = make_stmt();
                 	$$->breakList = mergelist($1->breakList, $2->breakList);
                 	$$->contList = mergelist($1->contList, $2->contList);
		     }

     |stmt { 
     		$$ = make_stmt();
     		$$->breakList = $1->breakList;
     		$$->contList = $1->contList;
	    }
	    
     |  
     ;

quadsave: {$$ = nextquadlabel();}

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
						 if(*$2 != IF_EQ && *$2 != IF_NOTEQ) yyerror("Invalid type for compare expression");
				   	  }else if($3->sym->gramType != gr_integer && $3->sym->gramType != gr_constinteger && $3->sym->gramType != gr_constreal && $3->sym->gramType  != gr_real){
						if(*$2 != IF_EQ && *$2 != IF_NOTEQ) yyerror("Invalid type for compare expression");
    					  } 
	    					  $$->truelist = newlist(nextquadlabel());
	    					  $$->falselist = newlist(nextquadlabel()+1);
	    					  
	    					  emit(*$2,$1,$3,NULL,0,0);
						  emit(JUMP,NULL,NULL,NULL,0,0); 
					  
					  
					 }
    | expr boolop quadsave expr %prec AND {$$ = makeExpression(boolexpr_e,newtemp(),NULL,NULL);
				 
				 /* if($1->type != boolexpr_e){
				  	$1->truelist = newlist(nextquadlabel());
					$1->falselist = newlist(nextquadlabel() + 1);
					emit(IF_EQ,$1,newexpr_constbool(1),NULL,0,0);
					emit(JUMP,NULL,NULL,NULL,0,0);
				  }*/

				  if($4->type != boolexpr_e){
				  	$4->truelist = newlist(nextquadlabel());
					$4->falselist = newlist(nextquadlabel() + 1);
					emit(IF_EQ,$4,newexpr_constbool(1),NULL,0,0);
					emit(JUMP,NULL,NULL,NULL,0,0);
				  }

    				  if(*$2 == OP_AND){
    				  	patchlist($1->truelist, $3);
    				  	$$->truelist = $4->truelist;
    				  	$$->falselist = mergelist($1->falselist, $4->falselist);
    				  } else if(*$2 == OP_OR){
    				  	patchlist($1->falselist, $3);
    				  	$$->truelist = mergelist($1->truelist, $4->truelist);
    				  	$$->falselist = $4->falselist;
    				  }
				 //emit(*$2, newexpr_constbool($1->sym->boolVal), newexpr_constbool($3->sym->boolVal), $$, 0, 0);
				 }
    | term { $$ = makeExpression($1->type,$1->sym,$1->index,NULL);
    		$$->truelist = $1->truelist;
		$$->falselist = $1->falselist;
	   }
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
		  emit(UMINUS,$2,NULL,$$,0,0);
		 }
    | NOT expr {
		
		if($2->type == boolexpr_e){
			printf("boo\n");
			$$ = makeExpression(boolexpr_e,newtemp(),NULL,NULL);
			$$->truelist = $2->falselist;
			$$->falselist = $2->truelist;
			//emit(OP_NOT,$2,NULL,$$,0,0);
		}else{
			printf("nah bro\n");
			$2->truelist = newlist(nextquadlabel());
			$2->falselist = newlist(nextquadlabel() + 1);
			emit(IF_EQ,$2,newexpr_constbool(1),NULL,0,0);
			emit(JUMP,NULL,NULL,NULL,0,0);
			
		}
	       }
    | PLUSPLUS lvalue {checkArithmetic($2);
		       	if($2->type == tableitem_e){
				$$ = emit_iftableitem($2);
				emit(ADD,$$,newexpr_constnum(1),$$,0,0);
				emit(TABLESETELEM,$2,$2->index,$$,0,0);
			}else{
				emit(ADD,$2,newexpr_constnum(1),$2,0,0);
				$$ = makeExpression(arithexpr_e,newtemp(),NULL,NULL);
				//$$->sym = newtemp();
				emit(ASSIGN,$2,NULL,$$,0,0);
			}
		      }
    | lvalue PLUSPLUS {
    			 checkArithmetic($1);
			 $$ = makeExpression(arithexpr_e,newtemp(),NULL,NULL);
			 //$$->sym = newtemp();

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
				 $$ = makeExpression(arithexpr_e,newtemp(),NULL,NULL);
				 //$$->sym = newtemp();
				 emit(ASSIGN,$2,NULL,$$,0,$2->sym->value.varVal->line);
			 }
			}
    | lvalue MINUSMINUS {
    			 checkArithmetic($1);
			 $$ = makeExpression(arithexpr_e,newtemp(),NULL,NULL);
			 //$$->sym = newtemp();
			
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
    | primary {$$ = makeExpression($1->type,$1->sym, $1->index,NULL);
    	       $$->truelist = $1->truelist;
	       $$->falselist = $1->falselist;
	      }
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
											if($3->sym->gramType == gr_conststring){ 
												$1->sym->grammarVal.string = strdup($3->sym->grammarVal.string); 
												$1->sym->gramType = gr_string; 
											}
											else if($3->sym->gramType == gr_string) {
												$1->sym->grammarVal.string = strdup($3->sym->grammarVal.string);
											}
											else if($3->sym->gramType == gr_boolean) $1->sym->grammarVal.boolean = $3->sym->grammarVal.boolean;
											else if($3->sym->gramType == gr_nil) $1->sym->grammarVal.nil = $3->sym->grammarVal.nil;
											else if($3->sym->gramType == gr_integer) $1->sym->grammarVal.intNum = $3->sym->grammarVal.intNum;
											else if($3->sym->gramType == gr_constinteger) {
												$1->sym->grammarVal.intNum = $3->sym->grammarVal.intNum;
												$1->sym->gramType = gr_integer;
											}
											else if($3->sym->gramType == gr_real) $1->sym->grammarVal.realNum = $3->sym->grammarVal.realNum;
											else if($3->sym->gramType == gr_constreal){
												$1->sym->grammarVal.realNum = $3->sym->grammarVal.realNum;
												$1->sym->gramType = gr_real;
											}
											else if($3->sym->gramType == gr_funcaddr) $1->sym->grammarVal.funcPtr = $3->sym->grammarVal.funcPtr;
											$1->sym->boolVal = $3->sym->boolVal;
											//$1->type = $3->type;
				
										/*	emit(ASSIGN,$3, NULL, $1,0,0);
											$$ = makeExpression(assignexpr_e,$1->sym,$1->index,NULL); 
											$$->sym = newtemp();
											$$->sym->gramType = $3->sym->gramType;
										*/	
											
											if($3->type == boolexpr_e){
												
												$$ = makeExpression(assignexpr_e,$1->sym,$1->index,NULL); 
												$$->sym = newtemp();
												$$->sym->gramType = $3->sym->gramType;
												
												patchlist($3->truelist, nextquadlabel());
												emit(ASSIGN,newexpr_constbool(1),NULL,$$,0,0);
								  				emit(JUMP,NULL,NULL,NULL,nextquadlabel() + 2,0);
								  				patchlist($3->falselist, nextquadlabel());
								  				emit(ASSIGN,newexpr_constbool(0),NULL,$$,0,0);
											 	emit(ASSIGN, $$, NULL, $1, 0, 0);
											 	
											 } else {
											 	emit(ASSIGN, $3, NULL, $1, 0, 0);
											 	$$ = makeExpression(assignexpr_e,$1->sym,$1->index,NULL); 
												$$->sym = newtemp();
												$$->sym->gramType = $3->sym->gramType;
											 	emit(ASSIGN, $1, NULL, $$, 0, 0);
											 }
											
											
											
											
									   }
								}
							}
	  ;

primary: lvalue {$$ = emit_iftableitem($1);}
       | call
       | objectdef  {$$ = makeExpression($1->type,$1->sym,$1->index,NULL);} 
       | PARENTHOPEN funcdef PARENTHCLOSE { $$ = makeExpression($2->type,$2,NULL,NULL); }
       | const {$$ = makeExpression($1->type,$1->sym,$1->index,NULL); }
       ;

lvalue: IDENTIFIER		{
							if(libFuncCheck($1)){ 
								SymbolTableEntry_t *res = upStreamLookUp(currScope, $1);
								if(res != NULL){
									if(res->type == libfunc )yyerror("Redifinition of token");	
									else if(res->type != global){
										if(res->unionType == unionVar){ 
											if(res->value.varVal->scope != currScope){
											//	yyerror("Not accesible variable");  //TODO: Check for loops

											}
										}
									}
									if(res->gramType == gr_conststring) $$ = makeExpression(conststring_e,res,NULL,NULL);
									else if(res->gramType == gr_string) $$ = makeExpression(var_e,res,NULL,NULL);
									else if(res->gramType == gr_boolean) $$ = makeExpression(var_e,res,NULL,NULL);
									else if(res->gramType == gr_nil) $$ = makeExpression(nil_e,res,NULL,NULL);
									else if(res->gramType == gr_integer) $$ = makeExpression(var_e,res,NULL,NULL);
									else if(res->gramType == gr_constinteger) $$ = makeExpression(constnum_e,res,NULL,NULL);
									else if(res->gramType == gr_real) $$ = makeExpression(var_e,res,NULL,NULL);
									else if(res->gramType == gr_constreal) $$ = makeExpression(constnum_e,res,NULL,NULL);
									else if(res->gramType == gr_funcaddr) $$ = makeExpression(programfunc_e,res,NULL,NULL);
									else $$ = makeExpression(var_e, res,NULL,NULL);
								}
								else{ (currScope == 0) ? (res = makeVariableEntry($1,global)) : (res = makeVariableEntry($1,local));
									$$ = makeExpression(var_e,res,NULL,NULL);
									
									
								}
							}else { printf("lala1\n"); $$ = makeExpression(libraryfunc_e, upStreamLookUp(currScope, $1), NULL, NULL);}
						}
						
     
	  | LOCAL IDENTIFIER	{ 	
	  				if(libFuncCheck($2)){
						SymbolTableEntry_t* res = scopeLookUp(currScope, $2);
						if(res == NULL){
							res = makeVariableEntry($2,local);
							$$ = makeExpression(var_e,res,NULL,NULL);
						}else{
							
							if(res->gramType == gr_conststring) $$ = makeExpression(conststring_e,res,NULL,NULL);
							else if(res->gramType == gr_string) $$ = makeExpression(var_e,res,NULL,NULL);
							else if(res->gramType == gr_boolean) $$ = makeExpression(var_e,res,NULL,NULL);
							else if(res->gramType == gr_nil) $$ = makeExpression(nil_e,res,NULL,NULL);
							else if(res->gramType == gr_integer) $$ = makeExpression(var_e,res,NULL,NULL);
							else if(res->gramType == gr_constinteger) $$ = makeExpression(constnum_e,res,NULL,NULL);
							else if(res->gramType == gr_real) $$ = makeExpression(var_e,res,NULL,NULL);
							else if(res->gramType == gr_constreal) $$ = makeExpression(constnum_e,res,NULL,NULL);
							else if(res->gramType == gr_funcaddr) $$ = makeExpression(programfunc_e,res,NULL,NULL);
						}
					} else {
						$$ = makeExpression(libraryfunc_e, scopeLookUp(currScope, $1), NULL, NULL);
					       printf("lala2\n");
						   }
				}     
      | DOUBLECOLON IDENTIFIER	{ 
      				  SymbolTableEntry_t* res = scopeLookUp(0,$2); 	
      				  if(res == NULL){ 
      					 yyerror("Global Variable not found");
					 $$ = makeExpression(nil_e, NULL, NULL, NULL);
				  }else{
					if(res->gramType == gr_conststring) $$ = makeExpression(conststring_e,res,NULL,NULL);
					else if(res->gramType == gr_string) $$ = makeExpression(var_e,res,NULL,NULL);
					else if(res->gramType == gr_boolean) $$ = makeExpression(var_e,res,NULL,NULL);
					else if(res->gramType == gr_nil) $$ = makeExpression(nil_e,res,NULL,NULL);
					else if(res->gramType == gr_integer) $$ = makeExpression(var_e,res,NULL,NULL);
					else if(res->gramType == gr_constinteger) $$ = makeExpression(constnum_e,res,NULL,NULL);
					else if(res->gramType == gr_real) $$ = makeExpression(var_e,res,NULL,NULL);
					else if(res->gramType == gr_constreal) $$ = makeExpression(constnum_e,res,NULL,NULL);
					else if(res->gramType == gr_funcaddr) $$ = makeExpression(programfunc_e,res,NULL,NULL);
				  }
				}
      | member {$$ = makeExpression($1->type,$1->sym, $1->index, NULL);}
      ;

call_lvalue: IDENTIFIER				{
								SymbolTableEntry_t *res = upStreamLookUp(currScope,$1);
								  if(res != NULL) {
                                                                        if((res->type != userfunc && res->type != libfunc) && (res->gramType != gr_funcaddr)) yyerror("Function not found");
                                                                        else {
                                                                                printf("calling function %s\n",res->grammarVal.funcPtr->symbol->name);
                                                                                if(res->type == libfunc){
                                                                                         $$ = makeExpression(libraryfunc_e,res,NULL,NULL);
                                                                                }else {
                                                                                         $$ = makeExpression(programfunc_e,res,NULL,NULL);
                                                                                 }
                                                                        }
                                                                 }else{
                                                                 $$ = makeExpression(nil_e,NULL,NULL,NULL);
                                                                 yyerror("Function not found");

                                                                 }
								
								 
								 
						}		
	   | DOUBLECOLON IDENTIFIER	{SymbolTableEntry_t *res = scopeLookUp(0,$2);
								 if(res != NULL){
									if((res->type != userfunc && res->type != libfunc) && (res->gramType != gr_funcaddr)) yyerror("Function not found");
									else {
										printf("caling function %s\n", res->value.varVal->name);
										if(res->type == libfunc){
                                                                                         $$ = makeExpression(libraryfunc_e,res,NULL,NULL);
                                                                                }else {
                                                                                         $$ = makeExpression(programfunc_e,res,NULL,NULL);
                                                                                 }
									}
								}else {
									yyerror("Function not found");
									$$ = makeExpression(nil_e,NULL,NULL,NULL);
								}
					}
								
	   | member {$$ = makeExpression($1->type,$1->sym,$1->index,0);}
	   ;

member: lvalue DOT IDENTIFIER {$$ = member_item($1,$3);}
      | lvalue SQBRACKETOPEN expr SQBRACKETCLOSE {
      						  $1 = emit_iftableitem($1);
      						  $$ = makeExpression(tableitem_e,$1->sym,$3,NULL);
						 }
      | call DOT IDENTIFIER{
      				$$ = member_item($1, $3);	
      			   }
      | call SQBRACKETOPEN expr SQBRACKETCLOSE {
      							$1 = emit_iftableitem($1);
							$$ = makeExpression(tableitem_e, $1->sym, $3, NULL);
      					       }
      ;

call: call PARENTHOPEN {elistFlag = 0;}elist PARENTHCLOSE {
    					   $$ = makeCall($$,$4);
						//elistFlag = 0; 
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
			  	if($2->method){
			  	
			 	struct exprNode* new = malloc(sizeof(struct exprNode));
				new->node = $1;
				new->next = $2->elist;
				$2->elist = new;
				
				
				$1 = emit_iftableitem(member_item($1,$2->name));
			  } 
			 
			  $$ = makeCall($1,$2->elist);
			  //elistFlag = 0;

			  }
    		
    | PARENTHOPEN funcdef PARENTHCLOSE PARENTHOPEN {elistFlag = 0;}elist PARENTHCLOSE {
    								      	struct expr* func = makeExpression(programfunc_e,$2,NULL,NULL);
									$$ = makeCall(func,$6);
									//elistFlag = 0;
								      }
    ;

callsuffix: normcall { $$ = $1; }
	  | methodcall { $$ = $1; }
	  ;

normcall: PARENTHOPEN {elistFlag = 0;} elist PARENTHCLOSE {/*elistFlag = 0;*/$$ = malloc(sizeof(struct call)); $$->elist = $3; $$->method  = 0; $$->name = NULL; }
	;

methodcall: DOUBLEDOT IDENTIFIER PARENTHOPEN {elistFlag = 0;}elist PARENTHCLOSE {/*elistFlag = 0;*/ $$ = malloc(sizeof(struct call)); $$->elist = $5; $$->method  = 1; $$->name = strdup($2); }
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


objectdef: SQBRACKETOPEN {elistFlag = 0;}elist SQBRACKETCLOSE {
	 						//	elistFlag = 0;
								struct expr* t = makeExpression(newtable_e,newtemp(),NULL,NULL); 
								t->sym->boolVal = 1;
								emit(TABLECREATE,NULL,NULL,t,0,0);
								
								struct exprNode* head = $3;
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
								t->sym->boolVal = 1;
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

block: CURBRACKETOPEN {currScope++; allocateScopes(currScope);} stmts CURBRACKETCLOSE {hideScope(currScope);currScope--; $$ = make_stmt();$$ = $3;}  
     ;



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
					push_scopeoffset(currscopeoffset());
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
					int offset = popandtop();
					restorecurrscopeoffset(offset);
					$1->value.funcVal->arglist = $2;
					printf("%s\n", $1->value.funcVal->name);
					$$ = $1;
					struct expr* newExpr = makeExpression(programfunc_e, $1, NULL, NULL);
					emit(FUNCEND, NULL, NULL, newExpr, 0, 0);
				      }
       					
       ;

const: number {$$ = malloc(sizeof(struct expr)); $$->sym = $1;
		$$->type = constnum_e; $$->index = NULL; $$->next = NULL;  } 
     | STRING {$$ = malloc(sizeof(struct expr)); $$->type = conststring_e;$$->sym = malloc(sizeof(SymbolTableEntry_t));$$->sym->symbol = malloc(sizeof(struct sym)); 
     		$$->sym->gramType = gr_conststring; 
     		$$->sym->symbol->name = "const_string"; $$->sym->grammarVal.string = malloc(strlen($1)+1); 
     		strcpy($$->sym->grammarVal.string, $1); 
		$$->sym->boolVal = (strcmp($1, "") == 0) ? 0 : 1;
		$$->type = conststring_e; $$->index = NULL; $$->next = NULL;}
     | NIL {$$ = malloc(sizeof(struct expr)); $$->sym = malloc(sizeof(SymbolTableEntry_t));$$->sym->symbol = malloc(sizeof(struct sym)); $$->sym->gramType = gr_nil; 
     		$$->sym->symbol->name = "nil"; $$->sym->grammarVal.nil = 1; $$->sym->boolVal = 0; $$->type = nil_e; $$->index = NULL;
     		$$->next = NULL;}
     | TRUE {$$ = malloc(sizeof(struct expr)); $$->sym = malloc(sizeof(SymbolTableEntry_t)); $$->sym->symbol = malloc(sizeof(struct sym));$$->sym->gramType = gr_boolean; 
     		$$->sym->symbol->name = "const_true"; $$->sym->grammarVal.boolean = 1; $$->sym->boolVal = 1; $$->type = constbool_e; 
     		$$->index = NULL; $$->next = NULL;}
     | FALSE {$$ = malloc(sizeof(struct expr)); $$->sym = malloc(sizeof(SymbolTableEntry_t)); $$->sym->symbol = malloc(sizeof(struct sym));$$->sym->gramType = gr_boolean;
     		$$->sym->symbol->name = "const_false";$$->sym->grammarVal.boolean = 0; $$->sym->boolVal = 0; $$->type = constbool_e; 
     		$$->index = NULL;$$->next = NULL;}
     ;

number: INTEGER {$$ = malloc(sizeof(SymbolTableEntry_t));
                    int length = snprintf(NULL,0,"%d",$1);
                    $$->symbol = malloc(sizeof(struct sym));
                    $$->symbol->name = malloc(length + 1);
                    snprintf($$->symbol->name,length + 1,"%d",$1);
                    $$->gramType = gr_integer;
                    $$->grammarVal.intNum = $1;
		    $$->boolVal = ($1 == 0) ? 0 : 1;
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
		    		 $$->boolVal = ($1 == 0) ? 0 : 1;
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
				 makeVariableEntry($3, formal);}
      |
      ;

ifprefix: IF PARENTHOPEN expr PARENTHCLOSE {
						
						if($3->type == boolexpr_e){
							
							patchlist($3->truelist, nextquadlabel());
							emit(ASSIGN,newexpr_constbool(1),NULL,$3,0,0);
			  				emit(JUMP,NULL,NULL,NULL,nextquadlabel() + 2,0);
			  				patchlist($3->falselist, nextquadlabel());
			  				emit(ASSIGN,newexpr_constbool(0),NULL,$3,0,0);
						 	
						 }
						
						emit(IF_EQ, $3, newexpr_constbool(1), NULL, nextquadlabel() + 2, 0);
						
						$$ = nextquadlabel();

						emit(JUMP, NULL, NULL, NULL, 0, 0);
					  }
elseprefix: ELSE{
						$$ = nextquadlabel();
						emit(JUMP, NULL, NULL, NULL, 0, 0);
				}


if: ifprefix stmt { 				$$ = make_stmt(); 
						$$->breakList = $2->breakList;
						$$->contList = $2->contList; 
						patchlabel($1, nextquadlabel());
					}

	| ifprefix stmt elseprefix stmt{
										$$ = make_stmt(); 
										$$->breakList = mergelist($2->breakList, $4->breakList); 
										$$->contList = mergelist($2->contList, $4->contList); 
										patchlabel($1, $3+1);
										patchlabel($3, nextquadlabel());
																
									}
	;



loopstart:	{++loopcounter;}

loopend:	{--loopcounter;}

loopstmt: loopstart stmt loopend	{
						
						$$ = make_stmt();
						$$ = $2;
					}

whilestart: WHILE {
		    $$ = nextquadlabel();  
		  }

whilecond: PARENTHOPEN {currScope++; allocateScopes(currScope); } expr PARENTHCLOSE  {
	 				   currScope--;
	 				   
	 				   if($3->type == boolexpr_e){
							
							patchlist($3->truelist, nextquadlabel());
							emit(ASSIGN,newexpr_constbool(1),NULL,$3,0,0);
			  				emit(JUMP,NULL,NULL,NULL,nextquadlabel() + 2,0);
			  				patchlist($3->falselist, nextquadlabel());
			  				emit(ASSIGN,newexpr_constbool(0),NULL,$3,0,0);
						 	
					   }
	 				   
					   emit(IF_EQ,$3,newexpr_constbool(1),NULL,nextquadlabel() + 2,0);
					   $$ = nextquadlabel();
					   emit(JUMP,NULL,NULL,NULL,0,0);
	 				 }

while: whilestart whilecond loopstmt {
     					emit(JUMP,NULL,NULL,NULL,$1,0);
					patchlabel($2,nextquadlabel());
					
					$$ = make_stmt(); 
					$$->breakList = $3->breakList; 
					$$->contList = $3->contList;
										
					patchlist($3->breakList, nextquadlabel());
					patchlist($3->contList, $1);

       				      }

unfinjmp:{ $$ = nextquadlabel(); emit(JUMP,NULL,NULL,NULL,0,0); }

forretlabel: { $$ = nextquadlabel(); }


forprefix: FOR PARENTHOPEN {currScope++; allocateScopes(currScope); } {elistFlag = 0; } elist {/*elistFlag = 0;*/} SEMICOLON forretlabel expr SEMICOLON {
	 							       $$ = malloc(sizeof(struct for_labels));
	 							       $$->test = $8;
								       $$->enter = nextquadlabel();
								       
								       if($9->type == boolexpr_e){
							
										patchlist($9->truelist, nextquadlabel());
										emit(ASSIGN,newexpr_constbool(1),NULL,$9,0,0);
						  				emit(JUMP,NULL,NULL,NULL,nextquadlabel() + 2,0);
						  				patchlist($9->falselist, nextquadlabel());
						  				emit(ASSIGN,newexpr_constbool(0),NULL,$9,0,0);
									 	
									 }
								       emit(IF_EQ,$9,newexpr_constbool(1),NULL,nextquadlabel() + 5,0);
								}
forpostfix: forprefix unfinjmp {elistFlag = 0;}elist PARENTHCLOSE {currScope--; /*elistFlag = 0;*/} unfinjmp loopstmt unfinjmp {
   								    	   patchlabel($1->enter,$7 + 1);
								    	   patchlabel($2,nextquadlabel());
								    	   patchlabel($7,$1->test);
								    	   patchlabel($9,$2 + 1);

 									   $$ = make_stmt(); 
									   $$->breakList = $8->breakList;
									   $$->contList = $8->contList; 

								    	   patchlist($8->breakList, nextquadlabel());
								    	   patchlist($8->contList, $2 + 1);
   								  	 }

for: forpostfix {
			 $$ = make_stmt(); 
			$$->breakList = $1->breakList; 
			$$->contList = $1->contList; 
		}


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
    exit(-1);
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
    	
    	expand();
	expand_instructions();
	

    	    	
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
	
	
	generate_instructions();
	printInstructions();
	
	printValArray();

	run_alphaprogram();

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
	assert(quadNo < currQuad + 1);
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
	assert(total == currQuad - 1);
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

	if(currQuad - 1 == total){
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
     struct exprNode/* * */  *prev = NULL, *current = head, *next = NULL;
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
	
	for(int i =1; i < currQuad;i++){
		

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
 
	newExpr->truelist = 0;
	newExpr->falselist = 0;

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
	newentry->boolVal = value;
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
	s->breakList = 0;
	s->contList = 0;
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
		list = next;
	}
}

void push_scopeoffset(unsigned currscopeoff){
	struct scopeoffsetstack* new = malloc(sizeof(struct scopeoffsetstack));
	new->offset = currscopeoff;
	new->next = NULL;
	if(sos_top == 0){
		sos_top = new;
		sos_bottom = new;
	}else{
		sos_top->next = new;
		sos_top = new;
	}
}

int popandtop(void){
	if(sos_top == 0){
		return 0;
	}

	if(sos_top == sos_bottom){
		//free(sos_top);
		int temp = sos_top->offset;

		sos_top = 0;
		sos_bottom = 0;
		
		return temp;
	}
	
	struct scopeoffsetstack* temp = sos_bottom;
	struct scopeoffsetstack* prev = NULL;
	
	while(temp->next != NULL){
		prev = temp;
		temp = temp->next;
	}
	
	sos_top = prev;
	//free(temp);
	return sos_top->offset;

}



/* PHASE 4,5 */

void expand_instructions(){
	assert(totalInstr == currInstr - 1);
	struct instruction* i = malloc(NEW_SIZE_INSTR);
	if(quads){
		memcpy(i,instructions,CURR_SIZE_INSTR);
		free(instructions);
	}
	instructions = i;
	totalInstr += EXPAND_SIZE_INSTR;

}

unsigned nextinstructionlabel(void){
	return currInstr;
}


void push_funcstack(Function_t* func){
	
	struct func_stack* new = malloc(sizeof(struct func_stack));
	new->func = func;
	new->next = NULL;
	if(head == NULL){
		head = new;
	}else{
		struct func_stack* temp = head;
		while(temp->next != NULL){
			temp = temp->next;
		}
		temp->next = new;
	}	

}

Function_t* pop_funcstack(void){
	if(head == NULL){
		return NULL;
	}else{
		struct func_stack* temp = head;
		struct func_stack* prev = NULL;
		while(temp->next != NULL){
			prev = temp;
			temp = temp->next;
		}
		if(prev != NULL){
			prev->next = NULL;
		}else{
			head = NULL;
		}
		return temp->func;
	}
}

Function_t* top_funcstack(void){
	if(head == NULL){
		return NULL;
	}else{
		struct func_stack* temp = head;
		while(temp->next != NULL){
			temp = temp->next;
		}
		printf("%p\n", temp->func->retList);
		return temp->func;
	}
}



unsigned consts_newstring(char* str){
	unsigned i = 0;
	if(strArray == NULL){
		strArray = malloc(sizeof(char*));
		strArray[i] = strdup(str);
	}else{
		while(i < strCounter){
			if(strcmp(strArray[i],str) == 0){
				return i;
			}
			i++;
		}
		strArray = realloc(strArray,sizeof(char*) * (i + 1));
		strArray[i] = strdup(str);
	}
	strCounter++;
	return i;

}

unsigned consts_newnumber(double n){
	unsigned i = 0;
	if(numArray == NULL){
		numArray = malloc(sizeof(double));
		numArray[i] = n;
	}else{
		for(i = 0;i < numCounter;i++){	
			if(numArray[i] == n){
				return i;
			}
		}
		numArray = realloc(numArray,sizeof(double) * (i + 1));
		numArray[i] = (double) n;
	}
	numCounter++;
	return i;
}

unsigned userfuncs_newfunc(SymbolTableEntry_t* s){
	unsigned i = 0;
	if(funcArray == NULL){
		funcArray = malloc(sizeof(Function_t*));
		funcArray[i] = malloc(sizeof(Function_t));
		funcArray[i]->name = strdup(s->value.funcVal->name);
		funcArray[i]->arglist = s->value.funcVal->arglist;
		funcArray[i]->scope = s->value.funcVal->scope;
		funcArray[i]->line = s->value.funcVal->line;
		funcArray[i]->qaddress = s->value.funcVal->qaddress;
		funcArray[i]->totallocals = s->value.funcVal->totallocals;
		//TODO taddress
	}else{
		while(i < funcCounter){
			if((strcmp(funcArray[i]->name,s->value.funcVal->name) == 0) && (funcArray[i]->qaddress == s->value.funcVal->qaddress)){
				return i;
			}
			i++;
		}
		funcArray = realloc(funcArray,sizeof(Function_t*) * (i + 1));
		funcArray[i] = malloc(sizeof(Function_t));
		funcArray[i]->name = strdup(s->value.funcVal->name);
		funcArray[i]->arglist = s->value.funcVal->arglist;
		funcArray[i]->scope = s->value.funcVal->scope;
		funcArray[i]->line = s->value.funcVal->line;
		funcArray[i]->qaddress = s->value.funcVal->qaddress;
		funcArray[i]->totallocals = s->value.funcVal->totallocals;

	}
	funcCounter++;
	return i;
}

unsigned libfuncs_newused(char* l){
	if(libfuncArray == NULL){
		libfuncArray = malloc(sizeof(char*) * 12);
		for(unsigned i = 0; i < 12;i++){
			libfuncArray[i] = NULL;
		}
		libfuncArray[0] = strdup(l);
		libfuncCounter++;
		return 0;
	}else{
		unsigned i = 0;
		while(i < libfuncCounter){
			if(strcmp(libfuncArray[i],l) == 0){
				return i;
			}
			i++;
		}

		libfuncArray[i] = strdup(l); 
		libfuncCounter++;
		return i;
	}
}

void make_operand(struct expr* e, struct vmarg* arg){
	if(e == NULL){
		arg->type = uninitialized_a;
		arg->val = 0;
		return;
	}
	switch(e->type){
		case var_e:
		case assignexpr_e:
		case tableitem_e:
		case arithexpr_e:
		case boolexpr_e:
		case newtable_e: {
			assert(e->sym);
			arg->val = e->sym->symbol->offset;
			switch(e->sym->symbol->scopeSpace){
				case program_var: arg->type = global_a; break;
				case function_loc: arg->type = local_a; break;
				case formal_arg: arg->type = formal_a; break;
				default: assert(0);
			}
			break;
		}

		case constbool_e: {
			arg->val = e->sym->boolVal; //TODO check dis
			arg->type = bool_a; break;
		}

		case conststring_e: {
			arg->val = consts_newstring(e->sym->grammarVal.string);
			arg->type = string_a; break;
		}

		case constnum_e: {
			arg->val = (e->sym->gramType == gr_integer || e->sym->gramType == gr_constinteger) ? (consts_newnumber(e->sym->grammarVal.intNum)) : consts_newnumber(e->sym->grammarVal.realNum);
			arg->type = number_a; break;
		}
		
		case nil_e: {
			arg->type = nil_a; break;
		}

		case programfunc_e: {
			arg->type = userfunc_a;
			//arg->val = e->sym->taddress; //TODO check dis too
			arg->val = userfuncs_newfunc(e->sym);
			break;
		}

		case libraryfunc_e: {
			arg->type = libfunc_a;
			arg->val = libfuncs_newused(e->sym->symbol->name);
			break;
		}

		default: assert(0);

	}
}

void reset_operand(struct vmarg* arg){
	make_operand(NULL,/*&*/arg);
}

void append_retList(Function_t* ret,unsigned label){
	struct return_list* new = malloc(sizeof(struct return_list));
	new->retLabel = label;
	new->next = NULL;
	if(ret->retList == NULL){
		ret->retList = new;
	}else{
		struct return_list* temp = ret->retList;
		while(temp->next != NULL){
			temp = temp->next;
		}
		temp->next = new;
	}
}

void backpatch_retlist(struct return_list* ret,unsigned label){
	struct return_list* temp = ret;
	while(temp != NULL){
		if(instructions[temp->retLabel].result.type == label_a){
			instructions[temp->retLabel].result.val = label;
		}else{
			yyerror("Label doesn't exist during backpatching the return list ");
		}
		temp = temp->next;
	}
}

void make_numberoperand(struct vmarg* arg, double val){
	arg->val = consts_newnumber(val);
	arg->type = number_a;
}

void make_booloperand(struct vmarg* arg, unsigned val){
	arg->val = val;
	arg->type = bool_a;
}

void make_retvaloperand(struct vmarg* arg){
	arg->type = retval_a;
	arg->val = -1;
}


void add_incomplete_jump(unsigned instrNo, unsigned iaddress){
	struct incomplete_jump* new = malloc(sizeof(struct incomplete_jump));
	new->instrNo = instrNo;
	new->iaddress = iaddress;
	new->next = NULL;
	
	if(ij_head == 0){
		ij_head = new;
	}else{
		struct incomplete_jump* temp = ij_head;
		while(temp->next != 0){
			temp = temp->next;
		}
		temp->next = new;
	}

	ij_total++;
}

unsigned currprocessedquad(){
	return processedQuads;
}

void patch_incomplete_jumps(){
	struct incomplete_jump* temp = ij_head;
	if(temp == 0){
		return;
	}
	unsigned i = 0;
	while(i < ij_total){
		if(temp->iaddress == currQuad){
			instructions[temp->instrNo].result.val = tcodeSize;
		}else{
			instructions[temp->instrNo].result.val = quads[temp->iaddress].taddress;
		}
		i++;
		temp = temp->next;
	}
}

void emit_instruction(struct instruction* i){
	if(currInstr - 1 == totalInstr){
		expand_instructions();
	}

	struct instruction* new = instructions + currInstr++;
	new->opcode = i->opcode;
	new->arg1 = i->arg1;
	new->arg2 = i->arg2;
	new->result = i->result;
	//new->srcLine = i->srcLine; TODO le line
	tcodeSize++;
}

void generate(enum vmopcode op,struct quad* q){
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = op;
	make_operand(q->arg1,&(t->arg1));
	make_operand(q->arg2,&(t->arg2));
	make_operand(q->result,&(t->result));
	q->taddress = nextinstructionlabel();
	emit_instruction(t);
}

void generate_relational(enum vmopcode op,struct quad* q){
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = op;
	//make_operand(q->result,&(t->result));
	make_operand(q->arg1,&(t->arg1));
	make_operand(q->arg2,&(t->arg2));

	t->result.type = label_a;
	printf("q label is: %u, currProcessedQuad is: %u\n",q->label,currprocessedquad());
	if(q->label < currprocessedquad()){
		t->result.val = quads[q->label].taddress;
	}else{
		add_incomplete_jump(nextinstructionlabel(),q->label);
	}
	q->taddress = nextinstructionlabel();
	emit_instruction(t);
}

void generate_ADD (struct quad* q) { generate(add_v,q); }
void generate_SUB (struct quad* q) { generate(sub_v,q); }
void generate_MUL (struct quad* q) { generate(mul_v,q); }
void generate_DIV (struct quad* q) { generate(div_v,q); }
void generate_MOD (struct quad* q) { generate(mod_v,q); }
void generate_NEWTABLE (struct quad* q) { generate(newtable_v,q); }
void generate_TABLEGETELEM (struct quad* q) { generate(tablegetelem_v,q); }
void generate_TABLESETELEM (struct quad* q) { generate(tablesetelem_v,q); }
void generate_ASSIGN (struct quad* q) { generate(assign_v,q); }
void generate_NOP (struct quad* q) {struct instruction* t; t->opcode = nop_v; emit_instruction(t);}   
void generate_JUMP (struct quad* q) { generate_relational(jmp_v,q); }
void generate_IF_EQ (struct quad* q) { generate_relational(jeq_v,q); }
void generate_IF_NOTEQ (struct quad* q) { generate_relational(jne_v,q); }
void generate_IF_GREATER (struct quad* q) { generate_relational(jgt_v,q); }
void generate_IF_GREATEREQ (struct quad* q) { generate_relational(jge_v,q); }
void generate_IF_LESS (struct quad* q) { generate_relational(jlt_v,q); }
void generate_IF_LESSEQ (struct quad* q) { generate_relational(jle_v,q); }


void generate_NOT (struct quad* q) { 
	q->taddress = nextinstructionlabel();
	struct instruction* t;

	t->opcode = jge_v;
	make_operand(q->arg1,&(t->arg1));
	make_booloperand(&(t->arg2),0);
	t->result.type = label_a;
	t->result.val = nextinstructionlabel() + 3;
	emit_instruction(t);

	t->opcode = assign_v;
	make_booloperand(&(t->arg1),0);
	reset_operand(&(t->arg2));
	make_operand(q->result,&(t->result));
	emit_instruction(t);

	t->opcode = jmp_v;
	reset_operand(&(t->arg1));
	reset_operand(&(t->arg2));
	

}


void generate_OR (struct quad* q) { generate(add_v,q); }


void generate_PARAM (struct quad* q) { 
	q->taddress = nextinstructionlabel();
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = pusharg_v;
	make_operand(q->arg1,&(t->arg1));
	make_operand(q->arg2,&(t->arg2));
	make_operand(q->result,&(t->result));
	emit_instruction(t);
}

void generate_CALL (struct quad* q) { 
	q->taddress = nextinstructionlabel();
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = call_v;
	make_operand(q->arg1,&(t->arg1));
	make_operand(q->arg2,&(t->arg2));
	make_operand(q->result,&(t->result));
	emit_instruction(t);
}

void generate_GETRETVAL (struct quad* q) { 
	q->taddress = nextinstructionlabel();
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = assign_v;
	make_operand(q->result,&(t->result));
	make_operand(q->arg2,&(t->arg2));
	make_retvaloperand(&(t->arg1));
	emit_instruction(t);
}

void generate_FUNCSTART (struct quad* q) { 
	SymbolTableEntry_t* f;
	f = q->result->sym;
	f->taddress = nextinstructionlabel();
	q->taddress = nextinstructionlabel();
	userfuncs_newfunc(f);

	push_funcstack(f->value.funcVal);
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = funcenter_v;
	make_operand(q->result,&(t->result));
	make_operand(q->arg1,&(t->arg1));
	make_operand(q->arg2,&(t->arg2));
	
	emit_instruction(t);

}
void generate_FUNCEND (struct quad* q) { 
	Function_t* f;
	f = pop_funcstack();
	backpatch_retlist(f->retList,nextinstructionlabel());
	q->taddress = nextinstructionlabel();
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = funcexit_v;
	make_operand(q->result,&(t->result));
	reset_operand(&(t->arg1));
	reset_operand(&(t->arg2));
	/*make_operand(q->arg1,&(t->arg1));
	make_operand(q->arg2,&(t->arg2));*/
	emit_instruction(t);
}
void generate_RETURN (struct quad* q) { 
	q->taddress = nextinstructionlabel();
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = assign_v;
	make_retvaloperand(&(t->result));
	make_operand(q->arg1,&(t->arg1));
	//make_operand(q->arg1,&(t->arg2));
	reset_operand(&(t->arg2));
	emit_instruction(t);
	Function_t* f = top_funcstack();
	printf("here: %p\n", f);
	
	append_retList(f,nextinstructionlabel());
	
	t->opcode = jmp_v;
	reset_operand(&(t->arg1));
	reset_operand(&(t->arg2));
	t->result.type = label_a;
	emit_instruction(t);
}

void generate_UMINUS(struct quad* q){ 
	q->taddress = nextinstructionlabel();
	struct instruction* t = malloc(sizeof(struct instruction));
	t->opcode = mul_v;
	make_operand(q->arg1,&(t->arg1));
	make_operand(newexpr_constnum(-1),&(t->arg2));
	make_operand(q->result,&(t->result));
	emit_instruction(t);
}

void generate_instructions(void){
	for(unsigned i = 1;i < currQuad;i++){
		//printf("%d\n", (quads + i)->op);
		(*generators[quads[i].op])(quads + i);
		processedQuads++;
	}
	
	patch_incomplete_jumps();
}


void printEnum(FILE* file,struct vmarg* arg){

        if(arg == NULL){
                fprintf(file,"%-25s","-");
                return;
        }


        switch(arg->type){
                case label_a:   fprintf(file,"%-25s","label_a");
                        break;
                case global_a:  fprintf(file,"%-25s","global_a");
                        break;
                case formal_a:  fprintf(file,"%-25s","formal_a");
                        break;
                case local_a:   fprintf(file,"%-25s","local_a");
                        break;
                case number_a:  fprintf(file,"%-25s","number_a");
                        break;
                case string_a:  fprintf(file,"%-25s","string_a");
                        break;
                case bool_a:    fprintf(file,"%-25s","bool_a");
                        break;
                case nil_a:     fprintf(file,"%-25s","nil_a");
                        break;
                case userfunc_a: fprintf(file,"%-25s","userfunc_a");
                        break;
                case libfunc_a: fprintf(file,"%-25s","libfunc_a");
                        break;
                case retval_a:  fprintf(file,"%-25s","retval_a");
                        break;

                case uninitialized_a:   fprintf(file,"%-25s","-");
                        break;

                default:
                        assert(0);
        }

}

void printInstructions(){
        FILE* file;
        file = fopen("instructions.txt","w");

        if(file == NULL){
                yyerror("Couldnt make instructions.txt file");
        }

        fprintf(file, "%-8s %-21s %-20s %-31s %-20s %-29s %-20s %-18s\n", "Instruction#", "opcode", "result", "result value", "arg1", "arg1 value", "arg2", "arg2 value");
fprintf(file,"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n");

        for(int i = 1; i < currInstr;i++){


                fprintf(file,"%-8d",i);
                /*:))))))))))*/
                switch (instructions[i].opcode){
                                case assign_v:
                                        fprintf(file,"%-25s ", "assign");break;
                                case add_v:
                                        fprintf(file,"%-25s ","add");break;
                                case sub_v:
                                        fprintf(file,"%-25s ","sub");break;
                                case mul_v:
                                        fprintf(file,"%-25s ","mul");break;
                                case div_v:
                                        fprintf(file,"%-25s ","div");break;
                                case mod_v:
                                        fprintf(file,"%-25s ","mod");break;
                                case uminus_v:
                                        fprintf(file,"%-25s ","uminus");break;
                                case and_v:
                                        fprintf(file,"%-25s ","and");break;
                                case or_v:
                                        fprintf(file,"%-25s ","or");break;
                                case not_v:
                                        fprintf(file,"%-25s ","not");break;
                                case jeq_v:
                                        fprintf(file,"%-25s ","if_eq");break;
                                case jne_v:
                                        fprintf(file,"%-25s ","if_noteq");break;
                                case jle_v:
                                        fprintf(file,"%-25s ","if_lesseq ");break;
                                case jge_v:
                                        fprintf(file,"%-25s ","if_greatereq");break;
                                case jlt_v:
                                        fprintf(file,"%-25s ","if_less");break;
                                case jgt_v:
                                        fprintf(file,"%-25s ","if_greater");break;
                                case call_v:
                                        fprintf(file,"%-25s ","call");break;
                                case pusharg_v:
                                        fprintf(file,"%-25s ","pusharg");break;
                                /*case ret_v:
                                        fprintf(file,"%-20s ","ret");break;
                                case GETRETVAL:
                                        fprintf(file,"%-20s ","getretval");break;*/
                                case funcenter_v:
                                        fprintf(file,"%-25s ","funcstart");break;
                                case funcexit_v:
                                        fprintf(file,"%-25s ","funcend");break;
                                case newtable_v:
                                        fprintf(file,"%-25s ","tablecreate");break;
                                case tablegetelem_v:
                                        fprintf(file,"%-25s ","tablegetelem");break;
                                case tablesetelem_v:
                                        fprintf(file,"%-25s ","tablesetelem");break;
                                case jmp_v:
                                        fprintf(file,"%-25s ","jump");break;
                                default:
                                        fprintf(file,"%-25s ","unknownopcode");break;
                        }

                        printEnum(file,&(instructions[i].result));
                        if(instructions[i].result.type != uninitialized_a){
                                fprintf(file,"%-25u ",instructions[i].result.val);
                        }else{
                                fprintf(file,"%-25s ", "-");
                        }

                        printEnum(file,&(instructions[i].arg1));
                        if(instructions[i].arg1.type != uninitialized_a){
                                fprintf(file,"%-25u ",instructions[i].arg1.val);
                        }else{
                                fprintf(file,"%-25s ", "-");
                        }

                        printEnum(file,&(instructions[i].arg2));
                        if(instructions[i].arg2.type != uninitialized_a){
                                fprintf(file,"%-25u ",instructions[i].arg2.val);
                        }else{
                                fprintf(file,"%-25s ", "-");
                        }





                fprintf(file,"  \n");
        }

        fclose(file);

}

void printValArray(){
	printf("\nString Array\n");
	for(unsigned i = 0;i < strCounter;i++){
		printf("%d:  %s\n",i,strArray[i]);
	
	}
	
	printf("\nNumber Array\n");
	for(unsigned i = 0;i < numCounter;i++){
		printf("%d:  %lf\n",i,numArray[i]);
	
	}
	
	printf("\nUserfunc Array\n");
	for(unsigned i = 0;i < funcCounter;i++){
		printf("%d:  %s\n",i,funcArray[i]->name);
	
	}

	printf("\nLibfunc Array\n");
	for(unsigned i = 0;i < libfuncCounter;i++){
		printf("%d:  %s\n",i,libfuncArray[i]);
	
	}

}


/* END OF PHASE 4 */

/* PHASE 5 */

double consts_getnumber(unsigned index){
	if(index > numCounter) yyerror("Index out of bounds on numArray ");

	return numArray[index];
}

char* consts_getstring(unsigned index){
	if(index > strCounter) yyerror("Index out of bounds on strArray ");

	return strArray[index];
}

char* libfuncs_getused(unsigned index){
	if(index > libfuncCounter) yyerror("Index out of bounds on libfuncArray ");

	return libfuncArray[index];
}

Function_t* userfuncs_getfunc(unsigned index){
	if(index > funcCounter) yyerror("Index out of bounds on funcArray ");

	return funcArray[index];
}

static void avm_initstack(void){
	for(unsigned i = 0;i < AVM_STACKSIZE;++i){
		AVM_WIPEOUT(stack[i]);
		stack[i].type = undef_m;
	}
}

void avm_tableincrefcounter(struct avm_table* t){
	++t->refCounter;
}

void avm_tabledecrefcounter(struct avm_table* t){
	assert(t->refCounter > 0);
	if(!--t->refCounter){
		avm_tabledestroy(t);
	}
}

void avm_tablebucketsinit(struct avm_table_bucket** p){
	for(unsigned i = 0;i < AVM_TABLE_HASHSIZE;++i){
		p[i] = (struct avm_table_bucket*)0;
	}
}

struct avm_table* avm_tablenew(void){
	struct avm_table* t = (struct avm_table*) malloc(sizeof(struct avm_table));
	AVM_WIPEOUT(*t);
	
	t->refCounter = t->total = 0;
	avm_tablebucketsinit(t->numIndexed);
	avm_tablebucketsinit(t->strIndexed);
	return t;
}

void avm_tablebucketsdestroy(struct avm_table_bucket** p){
	for(unsigned i = 0; i < AVM_TABLE_HASHSIZE;++i){
		for(struct avm_table_bucket* b = *p; b;){
			struct avm_table_bucket* del = b;
			b = b->next;
			avm_memcellclear(&(del->key));
			avm_memcellclear(&(del->value));
			free(del);
		}
		p[i] = (struct avm_table_bucket*) 0;
	}
}


void avm_tabledestroy(struct avm_table* t){
	avm_tablebucketsdestroy(t->strIndexed);
	avm_tablebucketsdestroy(t->numIndexed);
	free(t);
}

struct avm_memcell* avm_translate_operand(struct vmarg* arg,struct avm_memcell* reg){
	switch(arg->type){
		case global_a: return &stack[AVM_STACKSIZE - 1 - arg->val];
		case local_a:  return &stack[topsp - arg->val];
		case formal_a: return &stack[topsp + AVM_STACKENV_SIZE + 1 + arg->val];

		case retval_a: return &retval;

		case number_a: {
			if(reg == NULL){
				reg = malloc(sizeof(struct avm_memcell));
			}
			reg->type = number_m;
			reg->data.numVal = consts_getnumber(arg->val);
			return reg;
		}

		case string_a: {
			if(reg == NULL){
				reg = malloc(sizeof(struct avm_memcell));
			}
			reg->type = string_m;
			reg->data.strVal = strdup(consts_getstring(arg->val));
			return reg;
		}

		case bool_a: {
			if(reg == NULL){
				reg = malloc(sizeof(struct avm_memcell));
			}
			reg->type = bool_m;
			reg->data.boolVal = arg->val;
			return reg;
		}

		case nil_a: {
			if(reg == NULL){
				reg = malloc(sizeof(struct avm_memcell));
			}
			reg->type = nil_m; return reg;
		}

		case userfunc_a: {
			if(reg == NULL){
				reg = malloc(sizeof(struct avm_memcell));
			}
			reg->type = userfunc_m;
			reg->data.funcVal = userfuncs_getfunc(arg->val)->qaddress;
			return reg;
		}
		
		case libfunc_a:{
			if(reg == NULL){
				reg = malloc(sizeof(struct avm_memcell));
			}
			reg->type = libfunc_m;
			reg->data.libfuncVal = libfuncs_getused(arg->val);
			return reg;
		}

		default: assert(0);
	}

}

int execute_cycle(void){
	if(executionFinished)
		
		return 0;
	else
	if(pc == AVM_ENDING_PC){
		executionFinished = 1;
		
		return 0;
	}else{
		assert(pc < AVM_ENDING_PC);
		struct instruction* instr = instructions + pc;
		assert(instr->opcode >= 0 && instr->opcode <= AVM_MAX_INSTRUCTIONS);
		if(instr->srcLine)
			currLine = instr->srcLine;
		unsigned oldPC = pc;
		(*executeFuncs[instr->opcode])(instr);
		if(pc == oldPC)
			++pc;
		
		return 1;
	}
}

void execute_assign(struct instruction* t){
	struct avm_memcell* lv = avm_translate_operand(&(t->result),(struct avm_memcell*) 0);
	struct avm_memcell* rv = avm_translate_operand(&(t->arg1),&ax);

	assert(lv && ( &stack[AVM_STACKSIZE - 1] >= lv && lv > &stack[top] || lv == &retval));
	assert(rv);

	avm_assign(lv,rv);
}

/*void execute_add(struct instruction* t){}
void execute_sub(struct instruction* t){}
void execute_mul(struct instruction* t){}
void execute_div(struct instruction* t){}
void execute_mod(struct instruction* t){}*/

void execute_jeq(struct instruction* t){
	assert(t->result.type == label_a);

	struct avm_memcell* rv1 = avm_translate_operand(&(t->arg1), &ax);
	struct avm_memcell* rv2 = avm_translate_operand(&(t->arg2), &bx);

	unsigned char result = 0;

	if(rv1->type == undef_m || rv2->type == undef_m)
		avm_error("Undef involved in equality!",NULL);
	else
	if(rv1->type == bool_m || rv2->type == bool_m)
		result = (avm_tobool(rv1) == avm_tobool(rv2));
	else
	if(rv1->type == nil_m || rv2->type == nil_m)
		result = rv1->type == nil_m && rv2->type == nil_m;
	else
	if(rv1->type != rv2->type)
		avm_error("Types are different!",NULL);
	else {
		switch(rv1->type){
			case number_m: result = (rv1->data.numVal == rv2->data.numVal); break;
			
			case string_m: result = (strcmp(rv1->data.strVal,rv2->data.strVal) == 0);break;
			
			case table_m: result = (rv1->data.tableVal == rv2->data.tableVal);break;
			
			case userfunc_m: result = (rv1->data.funcVal == rv2->data.funcVal);break;
			
			case libfunc_m: result = (strcmp(rv1->data.libfuncVal,rv2->data.libfuncVal) == 0);break;
			
			default: assert(0);
		}
	}

	if(!executionFinished && result)
		pc = t->result.val;
}

void execute_jne(struct instruction* t){
	assert(t->result.type == label_a);

	struct avm_memcell* rv1 = avm_translate_operand(&(t->arg1), &ax);
	struct avm_memcell* rv2 = avm_translate_operand(&(t->arg2), &bx);

	unsigned char result = 0;

	if(rv1->type == undef_m || rv2->type == undef_m)
		avm_error("Undef involved in equality!",NULL);
	else
	if(rv1->type == bool_m || rv2->type == bool_m)
		result = (avm_tobool(rv1) == avm_tobool(rv2));
	else
	if(rv1->type == nil_m || rv2->type == nil_m)
		result = rv1->type == nil_m && rv2->type == nil_m;
	else
	if(rv1->type != rv2->type)
		avm_error("Types are different!",NULL);
	else {
		switch(rv1->type){
			case number_m: result = (rv1->data.numVal == rv2->data.numVal);break;
			
			case string_m: result = (strcmp(rv1->data.strVal,rv2->data.strVal) == 0);break;
			
			case table_m: result = (rv1->data.tableVal == rv2->data.tableVal);break;
			
			case userfunc_m: result = (rv1->data.funcVal == rv2->data.funcVal);break;
			
			case libfunc_m: result = (strcmp(rv1->data.libfuncVal,rv2->data.libfuncVal) == 0);break;
			
			default: assert(0);
		}
	}

	if(!executionFinished && !result)
		pc = t->result.val;

}
void execute_jle(struct instruction* t){
	struct avm_memcell* rv1 = avm_translate_operand(&(t->arg1),&ax);
	struct avm_memcell* rv2 = avm_translate_operand(&(t->arg2),&bx);
	
	if(rv1->type != number_m || rv2->type != number_m)
		avm_warning("Different types for jle",NULL);
	else
	if(rv1->data.numVal <= rv2->data.numVal)
		pc = t->result.val;
	

}
void execute_jge(struct instruction* t){
	struct avm_memcell* rv1 = avm_translate_operand(&(t->arg1),&ax);
	struct avm_memcell* rv2 = avm_translate_operand(&(t->arg2),&bx);
	
	if(rv1->type != number_m || rv2->type != number_m)
		avm_warning("Different types for jle",NULL);
	else
	if(rv1->data.numVal >= rv2->data.numVal)
		pc = t->result.val;

}
void execute_jlt(struct instruction* t){
	struct avm_memcell* rv1 = avm_translate_operand(&(t->arg1),&ax);
	struct avm_memcell* rv2 = avm_translate_operand(&(t->arg2),&bx);
	
	if(rv1->type != number_m || rv2->type != number_m)
		avm_warning("Different types for jle",NULL);
	else
	if(rv1->data.numVal < rv2->data.numVal)
		pc = t->result.val;

}
void execute_jgt(struct instruction* t){
	struct avm_memcell* rv1 = avm_translate_operand(&(t->arg1),&ax);
	struct avm_memcell* rv2 = avm_translate_operand(&(t->arg2),&bx);
	
	if(rv1->type != number_m || rv2->type != number_m)
		avm_warning("Different types for jle",NULL);
	else
	if(rv1->data.numVal > rv2->data.numVal)
		pc = t->result.val;

}

void execute_call(struct instruction* t){
	struct avm_memcell* func = avm_translate_operand(&(t->result), &ax);
	assert(func);
	
	switch(func->type) {
		case userfunc_m: {
			avm_callsaveenviroment();
			pc = func->data.funcVal;
			assert(pc < AVM_ENDING_PC);
			assert(code[pc].opcode == funcenter_v);
			break;
		}

		case string_m: {avm_calllibfunc(func->data.strVal); break;}
		case libfunc_m: {avm_calllibfunc(func->data.libfuncVal); break;}
		case table_m: {avm_call_functor(func->data.tableVal); break;}

		default: {
			char* s = avm_tostring(func);
			avm_error("call: cannot bind '%s' to function!",s);
			free(s);
			executionFinished = 1;
		}

	}
}

void execute_pusharg(struct instruction* t){
	struct avm_memcell* arg = avm_translate_operand(&(t->arg1), &ax);
	assert(arg);

	/* This is actually stack[top] = arg but we have to use avm_assign */
	avm_assign(&stack[top],arg);
	++totalActuals;
	avm_dec_top();
}

void execute_funcenter(struct instruction* t){
	struct avm_memcell* func  = avm_translate_operand(&(t->result),&ax);
	assert(func);
	assert(pc == func->data.funcVal); /*Func address should match PC*/
	
	/* Callee actions here. */
	totalActuals = 0;
	Function_t* funcInfo = avm_getfuncinfo(pc);
	topsp = top;
	top = top - funcInfo->totallocals;

}	

void execute_funcexit(struct instruction* t){
	unsigned oldTop = top;
	top = avm_get_envvalue(topsp + AVM_SAVEDTOP_OFFSET);
	pc = avm_get_envvalue(topsp + AVM_SAVEDPC_OFFSET);
	topsp = avm_get_envvalue(topsp + AVM_SAVEDTOPSP_OFFSET);

	while(++oldTop <= top) /*Intenionaly ignoring first*/
		avm_memcellclear(&stack[oldTop]);
}

void execute_newtable(struct instruction* t){
	struct avm_memcell* lv = avm_translate_operand(&(t->result),(struct avm_memcell*) 0);
	assert(lv && ( &stack[AVM_STACKSIZE - 1] >= lv && lv > &stack[top] || lv == &retval));

	avm_memcellclear(lv);

	lv->type = table_m;
	lv->data.tableVal = avm_tablenew();
	avm_tableincrefcounter(lv->data.tableVal);
}

void execute_tablegetelem(struct instruction* t){
	struct avm_memcell* lv = avm_translate_operand(&(t->result),(struct avm_memcell*) 0);
	struct avm_memcell* r = avm_translate_operand(&(t->arg1),(struct avm_memcell*) 0);
	struct avm_memcell* i = avm_translate_operand(&(t->arg2), &ax);

	assert(lv && &stack[AVM_STACKSIZE - 1] >= lv && lv > &stack[top] || lv == &retval);
	assert(r && &stack[AVM_STACKSIZE - 1] >= r && r > &stack[top] );
	assert(i);

	avm_memcellclear(lv);
	lv->type = nil_m; /*Default value*/

	if(r->type != table_m){
		avm_error("Incorrect table type",NULL);
	}
	else{
		struct avm_memcell* content = avm_tablegetelem(r->data.tableVal,i);
		if(content)
			avm_assign(lv,content);
		else {
			char* rs = avm_tostring(r);
			char* is = avm_tostring(i);
			avm_warning("%s not found",rs);
		}
	}
}

void execute_tablesetelem(struct instruction* t){
	struct avm_memcell* r = avm_translate_operand(&(t->result),(struct avm_memcell*) 0);
	struct avm_memcell* i = avm_translate_operand(&(t->arg1),&ax);
	struct avm_memcell* c = avm_translate_operand(&(t->arg2),&bx);

	assert(r && &stack[AVM_STACKSIZE - 1] >= r && r > &stack[top]);
	assert(i && c);

	if(r->type != table_m)
		avm_error("Incorrect table type",NULL);
	else
		avm_tablesetelem(r->data.tableVal,i,c);
}


void execute_nop(struct instruction* t){}

void avm_assign(struct avm_memcell* lv,struct avm_memcell* rv){
	if(lv == rv)	/*Same cells == destructive to assign*/
		return;

	if(lv->type == table_m && rv->type == table_m && lv->data.tableVal == rv->data.tableVal) /*Same tables == no need to assign*/
		return;

	if(rv->type == undef_m)	/*From undefined r-value == warn*/
		avm_warning("assigning from 'undef' content!",NULL);

	avm_memcellclear(lv);

	memcpy(lv,rv,sizeof(struct avm_memcell));

	/*Now take care of copied values or reference counting*/

	if(lv->type == string_m)
		lv->data.strVal = strdup(rv->data.strVal);
	else
	if(lv->type == table_m)
		avm_tableincrefcounter(lv->data.tableVal);

}

void avm_call_functor(struct avm_table* t){
	cx.type = string_m;
	cx.data.strVal = "()";
	struct avm_memcell* f = avm_tablegetelem(t,&cx);

	if(!f)
		avm_error("In calling table: no '()' element found",NULL);
	else
	if(f->type == table_m)
		avm_call_functor(f->data.tableVal);
	else
	if(f->type == userfunc_a){
		avm_push_table_arg(t);
		avm_callsaveenviroment();
		pc = f->data.funcVal;
		assert(pc < AVM_ENDING_PC && code[pc].opcode == funcenter_v);
	}else
		avm_error("In calling table: illegal '()' element value",NULL);

}

void avm_dec_top(void){
	if(!top){ /*Stack overflow*/
		avm_error("Stack Overflow",NULL);
		executionFinished = 1;
	}
	else
		--top;
	
}

void avm_push_envvalue(unsigned val){
	stack[top].type = number_m;
	stack[top].data.numVal = val;
	avm_dec_top();
}

void avm_callsaveenviroment(void){
	avm_push_envvalue(totalActuals);
	assert(code[pc].opcode == call_v);
	avm_push_envvalue(pc+1);
	avm_push_envvalue(top + totalActuals + 2);
	avm_push_envvalue(topsp);
}

unsigned avm_get_envvalue(unsigned i){
	assert(stack[i].type = number_m);
	unsigned val = (unsigned) stack[i].data.numVal;
	assert(stack[i].data.numVal == ((double) val));
	return val;
}

void avm_calllibfunc(char* id){
	SymbolTableEntry_t* libFuncEntry = scopeLookUp(0, id);
	if(libFuncEntry == NULL || libFuncEntry->unionType != unionFunc ||libFuncEntry->value.funcVal == NULL){
		avm_error("unsupported lib func '%s' called!",id);
		executionFinished = 1;
	}
	else{
		Function_t* f = libFuncEntry->value.funcVal;
		/*Notice that enter function and exit function are called manually!*/
		avm_callsaveenviroment();
		topsp = top; /*Enter function sequence. No stack locals. */
		totalActuals = 0;
		// (*f)();	/*Call library functions*/   TODO: to call the actual library function we implemented;
		if(!executionFinished) /*An error may naturally occur inside*/
			execute_funcexit((struct instruction*) 0); /*Return sequence*/
	}
}

unsigned avm_totalactuals(void){
	return avm_get_envvalue(topsp + AVM_NUMACTUALS_OFFSET);

}

struct avm_memcell* avm_getactual(unsigned i){
	assert(i < avm_totalactuals());
	return &stack[topsp + AVM_STACKENV_SIZE + 1 + i];

}

/* Implementation of the library function 'print'
   It displays every argument at the console
*/

void libfunc_print(void){
	unsigned n = avm_totalactuals();
	for(unsigned i = 0; i < n; ++i){
		char* s = avm_tostring(avm_getactual(i));
		puts(s);
		free(s);
	}
}


void avm_push_table_arg(struct avm_table* t){
	stack[top].type = table_m;
	avm_tableincrefcounter(stack[top].data.tableVal = t);
	++totalActuals;
	avm_dec_top();
}

char* avm_tostring(struct avm_memcell* m){
	assert(m->type >= 0 && m->type <= undef_m);
	return (*tostringFuncs[m->type])(m);
}

unsigned char avm_tobool(struct avm_memcell* m){
	switch(m->type){
		case number_m: return (m->data.numVal != 0);

		case string_m: return (strcmp(m->data.strVal,"") != 0);

		case bool_m: return m->data.boolVal;

		case table_m: return 1;

		case userfunc_m: return 1;

		case libfunc_m: return 1;

		case nil_m: return 0;

		case undef_m: return 0;

		default: assert(0);
	}
}

void execute_arithmetic(struct instruction* t){
	struct avm_memcell* lv = avm_translate_operand(&(t->result), (struct avm_memcell*) 0);
	struct avm_memcell* rv1 = avm_translate_operand(&(t->arg1), (struct avm_memcell*) 0);
	struct avm_memcell* rv2 = avm_translate_operand(&(t->arg2), (struct avm_memcell*) 0);

	assert(lv && (&stack[AVM_STACKSIZE - 1] >= lv && lv > &stack[top] || lv == &retval));
	assert(rv1 && rv2);

	if(rv1->type != number_m || rv2->type != number_m){
		avm_error("Not a number in arithmetic!",NULL);
		executionFinished = 1;

	}
	else {
		arithmetic_func_t op = arithmeticFuncs[t->opcode - add_v];
		avm_memcellclear(lv);
		lv->type = number_m;
		lv->data.numVal = (*op)(rv1->data.numVal,rv2->data.numVal);
	}
}

void execute_uminus(struct instruction* i){assert(0);}
void execute_and(struct instruction* i){assert(0);}
void execute_or(struct instruction* i){assert(0);}
void execute_not(struct instruction* i){assert(0);}

void execute_jmp(struct instruction* i){
	if(i->result.type != label_a) {
		avm_error("jump to an non label", ""); 
		executionFinished = 1;
	}

	pc = i->result.val;
}

double add_impl(double x,double y) { return x + y; }
double sub_impl(double x,double y) { return x - y; }
double mul_impl(double x,double y) { return x * y; }
double div_impl(double x,double y) { 
	if( y == 0){
		yyerror("Division with 0 ");	
	}
	return x / y; 
}

double mod_impl(double x,double y) { 
	if(y == 0){
		yyerror("Cant mod with 0 ");
	}
	return ((unsigned) x) % ((unsigned) y);
}

void avm_memcellclear (struct avm_memcell* m){
	if(m->type != undef_m){
		memclear_func_t f = memclearFuncs[m->type];

		if (f)
			(*f)(m);

		m->type = undef_m;
	
	}
}

void avm_error(char* format,char* v){
	fprintf(stderr, RED "AVM:EROR at runtime: %s about %s" RESET, format, v);
	executionFinished = 1;
}

void avm_warning(char* format,char* v){
	fprintf(stderr, PURPLE "AVM:EROR at runtime: %s about %s" RESET, format, v);
}


Function_t* avm_getfuncinfo(unsigned address){
	return userfuncs_getfunc(address);
}

void memclear_string(struct avm_memcell* m){
	assert(m->data.strVal);
	free(m->data.strVal);
}

void memclear_table(struct avm_memcell* m){
	assert(m->data.tableVal);
	avm_tabledecrefcounter(m->data.tableVal);
}

char* number_tostring(struct avm_memcell* m){
	int len = snprintf(NULL,0,"%lf",m->data.numVal) + 1;
	char* str = malloc(sizeof(char) * len);
	
	if(str == NULL){
		avm_error("Memory allocation failed",NULL);
	}

	sprintf(str,"%lf",m->data.numVal);
	return str;

}

char* string_tostring(struct avm_memcell* m){
	return strdup(m->data.strVal);
}

char* bool_tostring(struct avm_memcell* m){
	char* str = malloc(sizeof(char) * 10);
	
	if(m->data.boolVal == 1){
		str = "true";
	}else{
		str = "false";
	}

	return str;

}


char* table_tostring(struct avm_memcell* m){ //TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	return "nah bro";
}

char* userfunc_tostring(struct avm_memcell* m){
	assert(m->data.funcVal <= funcCounter);
	Function_t* f = funcArray[m->data.funcVal];
	return strdup(f->name);
}

char* libfunc_tostring(struct avm_memcell* m){
	return strdup(m->data.libfuncVal);	
}

char* nil_tostring(struct avm_memcell* m){
	char* str = "nil";
	return str;
}

char* undef_tostring(struct avm_memcell* m){
	char* str = "undef";
	return str;
}


unsigned int avm_table_hash(const char *pcKey){
	size_t ui;
	unsigned int uiHash = 0U;

	for (ui = 0U; pcKey[ui] != '\0'; ui++)
		uiHash = uiHash * HASH_MULTIPLIER + pcKey[ui];
	return uiHash % AVM_TABLE_HASHSIZE;
}

struct avm_table_bucket* getTableBucket(struct avm_table* table, struct avm_memcell* key){
	if(key->type != number_m && key->type != string_m){ avm_error("Invalid key type", "tablegetelem"); return NULL; }
	struct avm_table_bucket* list = (key->type == number_m) ? table->numIndexed[avm_table_hash(number_tostring(key))] : table->strIndexed[avm_table_hash(key->data.strVal)]; 
	/*Maybe convert numVal to string or find other solution same for below*/
	

	if(key->type == number_m){

		while(list != NULL){
			if(list->key.data.numVal == key->data.numVal)return list;
		
			list = list->next;
		}
	}else{
		while(list != NULL){
			if(strcmp(list->key.data.strVal, key->data.strVal) == 0)return list;
		
			list = list->next;
		}
	}

	avm_error("table element dont exist", "");
	return NULL;
}

void setTableBucket(struct avm_table* table,struct avm_table_bucket* bucket){
	if(bucket->key.type != number_m && bucket->key.type != string_m){avm_error("Invalid key type", "tablesetelem"); return;}
 struct avm_table_bucket** list = (bucket->key.type == number_m) ? &(table->numIndexed[avm_table_hash(number_tostring(&(bucket->key)))]) : &(table->strIndexed[avm_table_hash(bucket->key.data.strVal)]);

	bucket->next = *list;	
	*list = bucket;

}

struct avm_memcell* avm_tablegetelem(struct avm_table* table, struct avm_memcell* key){
		return &(getTableBucket(table,key)->value);
}

void avm_tablesetelem(struct avm_table* table,struct avm_memcell* index,struct avm_memcell* content){
	struct avm_table_bucket* bucket = malloc(sizeof(struct avm_table_bucket));
	bucket->key = *index;
	bucket->value = *content;
	bucket->next = NULL;
	
	setTableBucket(table,bucket);
}

void run_alphaprogram(void){
	while(execute_cycle());
}

/* END OF PHASE 5 */


SymbolTableEntry_t* makeLibEntry(char *name){

    	SymbolTableEntry_t *entry;
    	Function_t *f;

    	f = malloc(sizeof(Function_t));

	f->name = name;
	f->scope = currScope;
	f->line = 0;
	f->arglist = NULL;
	f->qaddress = 0;
	f->retList = NULL;

    	entry = malloc(sizeof(SymbolTableEntry_t));

    	assert(entry);

	entry->isActive = 1;
	entry->unionType = unionFunc;
	entry->value.funcVal = f;
	entry->type = libfunc;
	entry->gramType = gr_funcaddr;
	entry->boolVal = 1;

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
	entry->boolVal = 0;

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
	f->qaddress = 0;
	f->retList = NULL;

	f->arglist = makeFuncArgList(f->arglist,currScope);

	entry = malloc(sizeof(SymbolTableEntry_t));

	assert(entry);

	entry->isActive = 1;
	entry->unionType = unionFunc;
	entry->value.funcVal = f;
	entry->type = type;
	entry->gramType = gr_funcaddr;
	entry->boolVal = 1;

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
