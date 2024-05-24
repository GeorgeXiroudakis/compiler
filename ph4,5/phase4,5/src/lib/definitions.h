#define RESET   "\033[0m"
#define RED     "\033[31m"
#define unionVar   0
#define unionFunc  1

struct SymbolTableEntry;
struct sym;

enum grammar_type{
	gr_integer, gr_constinteger, gr_constreal, gr_real, gr_conststring, gr_string, gr_boolean, gr_nil, gr_funcaddr
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
	//struct FunctArgNode *prev;
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
		struct SymbolTableEntry* funcPtr;
	}grammarVal;

	short int boolVal;

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
	CALL, PARAM, RET, GETRETVAL, FUNCSTART, FUNCEND, TABLECREATE, TABLEGETELEM, TABLESETELEM, JUMP
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
	struct expr* next;
	
	int truelist;
	int falselist;
};

struct quad {
	enum iopcode op;
	struct expr* result;
	struct expr* arg1;
	struct expr* arg2;
	unsigned label;
	unsigned line;
};

struct exprNode {
	struct expr* node;
	struct exprNode* next;
};

struct call {
	struct expr* elist;
	unsigned char method;
	char* name;
};

struct indexed_elem{
	struct expr* index;
	struct expr* value;
	
	struct indexed_elem* next; 
};

struct for_labels{
	unsigned test;
	unsigned enter;
};


struct lc_stack_t{
	unsigned 	counter;
	struct		lc_stack_t* next;
};

struct stmt_t {
	int breakList,contList;
};

struct scopeoffsetstack {
	int 		offset;
	struct scopeoffsetstack* next;
};

/* PHASE 4,5 */

enum vmopcode {
	assign_v,	add_v,		sub_v,
	mul_v,		div_v,		mod_v,
	uminus_v,	and_v,		or_v,
	not_v,		jeq_v,		jne_v,
	jle_v,		jge_v,		jlt_v,
	jgt_v,		call_v,		pusharg_v,
	funcenter_v,	funcexit_v,	newtable_v,
	tablegetelem_v,	tablesetelem_v,	nop_v
};

enum vmarg_t {
	label_a		=0,
	global_a	=1,
	formal_a	=2,
	local_a		=3,
	number_a	=4,
	string_a	=5,
	bool_a		=6,
	nil_a		=7,
	userfunc_a	=8,
	libfunc_a	=9,
	retval_a	=10
};

struct vmarg {
	enum vmarg_t	type;
	unsigned	val;
};

struct instruction {
	enum vmopcode	opcode;
	struct vmarg	result;
	struct vmarg	arg1;
	struct vmarg	arg2;
	unsigned	srcLine;
};

struct userfunc {
	unsigned	address;
	unsigned	localSize;
	char*		id;
};

enum avm_memcell_t {
	number_m	=0,
	string_m	=1,
	bool_m		=2,
	table_m		=3,
	userfunc_m	=4,
	libfunc_m	=5,
	nil_m		=6,
	undef_m		=7
};

#define AVM_TABLE_HASHSIZE 211
struct avm_memcell;

struct avm_memcell {
	enum avm_memcell_t type;
	union {
		double			numVal;
		char* 			strVal;
		unsigned char		boolVal;
		struct avm_table* 	tableVal;
		unsigned 		funcVal;
		char*			libfuncVal;
	} data;
};

struct avm_table_bucket	{
	struct avm_memcell	key;
	struct avm_memcell	value;
	struct avm_table_bucket*	next;
};


struct avm_table{
	unsigned	refCounter;
	struct avm_table_bucket*	strIndexed[AVM_TABLE_HASHSIZE];
	struct avm_table_bucket*	numIndexed[AVM_TABLE_HASHSIZE];
	unsigned total;
};

#define AVM_STACKSIZE 4096
#define AVM_WIPEOUT(m)	memset(&(m), 0, sizeof(m))

struct incomplete_jump	{
	unsigned instrNo;
	unsigned iaddress;
	struct incomplete_jump* next;
};


