#define RESET   "\033[0m"
#define RED     "\033[31m"
#define unionVar   0
#define unionFunc  1

struct SymbolTableEntry;

typedef struct variable{
	const char *name;
	unsigned int scope;
	unsigned int line;

	/*union {
		int intNum;
		float realNum;
		char* string;
		short int boolean;
		char* nill;
	}value;*/
}Variable_t;

typedef struct FunctArgNode{
	struct SymbolTableEntry *arg;

	struct FunctArgNode *next;
}FunctArgNode_t;

typedef struct Function{
	const char *name;
	FunctArgNode_t *arglist;
	unsigned int scope;
	unsigned int line;
}Function_t;

enum SymbolType{
	global, local, formal, userfunc, libfunc
};

typedef struct SymbolTableEntry{
	short int isActive;
	short int unionType;
	union {
		Variable_t *varVal;
		Function_t *funcVal;
	}value;
	enum SymbolType type;
} SymbolTableEntry_t;

typedef struct scopeListNode{
    SymbolTableEntry_t *entry;
    struct scopeListNode *next;
} scopeListNode_t;

typedef struct ScopeArray{
    scopeListNode_t *head;
    scopeListNode_t *tail;
} ScopeArray_t;

/*PHASE 3*/

enum iopcode {
	assign, add, sub, mul, div, mod, uminus, and, or, not, if_eq, if_noteq, if_lesseq, if_greatereq, if_less, if_greater,
	call, param, ret, getretval, funcstart, funcand, tablecreate, tablegetelem, tablesetelem
};

enum expr_t {
	var_e, tableitem_e, 
	programfunc_e, libraryfunc_e, 
	arithexpr_e, boolexpr_e, assignexpr_e, newtable_e, 
	constnum_e, constbool_e, conststring_e, 
	nil_e
};

struct expr {
	expr_t type;
	//symbol* sym;  8a psaxnoume sto sym table kai me tis plhrofories tou symtableentry pou 8a exoume 8a arxikopoioume thn metavlhth tupou symbol TODO:na ftia3oume to struct symbol
	expr* index;
	double numConst;
	char* strConst;
	unsigned char boolConst;
	expr* next;
};

struct quad {
	iopcode op;
	expr* result;
	expr* arg1;
	expr* arg2;
	unsigned label;
	unsigned line;
};

quad* quads = (quad*)0;
unsigned total = 0;
unsigned int currQuad = 0;

#define EXPAND_SIZE 1024
#define CURR_SIZE (total*sizeof(quad))
#define NEW_SIZE (EXPAND_SIZE * sizeof(quad) + CURR_SIZE)