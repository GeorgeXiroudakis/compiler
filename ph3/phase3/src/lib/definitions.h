#define RESET   "\033[0m"
#define RED     "\033[31m"
#define unionVar   0
#define unionFunc  1

struct SymbolTableEntry;

typedef struct variable{
	const char *name;
	unsigned int scope;
	unsigned int line;
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

