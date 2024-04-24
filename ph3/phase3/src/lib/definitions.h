#define RESET   "\033[0m"
#define RED     "\033[31m"
#define unionVar   0
#define unionFunc  1

struct SymbolTableEntry;
struct sym;

enum grammar_type{
	gr_integer, gr_constinteger, gr_constreal, gr_real, gr_conststring, gr_string, gr_boolean, gr_nil
};

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
	}value*/
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
	unsigned int qaddress;
	unsigned int totallocals;
}Function_t;

enum SymbolType{
	global, local, temp, formal, userfunc, libfunc
};

typedef struct SymbolTableEntry{
	short int isActive;
	short int unionType;
	
	union {
		Variable_t *varVal;
		Function_t *funcVal;
	}value;
	
	enum SymbolType type;
	struct sym* symbol;
	enum grammar_type gramType;
	
	union {
		int intNum;
		float realNum;
		char* string;
		short int boolean;
		short int nil;
	}grammarVal;

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
#define EXPAND_SIZE 1024
#define CURR_SIZE (total*sizeof(struct quad))
#define NEW_SIZE (EXPAND_SIZE * sizeof(struct quad) + CURR_SIZE)

enum symbol_t { var_s, programfunc_s, libraryfunc_s };

enum scope_space { program_var, function_loc, formal_arg };

struct sym{
	enum symbol_t	type;
	char* 		name;
	enum scope_space scopeSpace;
	unsigned	offset;
};

enum iopcode {
	ASSIGN, ADD, SUB, MUL, DIV, MOD, UMINUS, OP_AND, OP_OR, OP_NOT, IF_EQ, IF_NOTEQ, IF_LESSEQ, IF_GREATEREQ, IF_LESS, IF_GREATER,
	CALL, PARAM, RET, GETRETVAL, FUNCSTART, FUNCEND, TABLECREATE, TABLEGETELEM, TABLESETELEM
};

enum expr_en {
	var_e, tableitem_e, 
	programfunc_e, libraryfunc_e, 
	arithexpr_e, boolexpr_e, assignexpr_e, newtable_e, 
	constnum_e, constbool_e, conststring_e, 
	nil_e
};

struct expr {
	enum expr_en type;
	SymbolTableEntry_t* sym;
	struct expr* index;
	/*double numConst;
	char* strConst;
	unsigned char boolConst;*/
	struct expr* next;
};

struct quad {
	enum iopcode op;
	struct expr* result;
	struct expr* arg1;
	struct expr* arg2;
	unsigned label;
	unsigned line;
};



