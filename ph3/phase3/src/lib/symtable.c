#include<stdlib.h>
#include<assert.h>
#include"symtable.h"
#include<stdio.h>
#include<string.h>

#define HASH_MULTIPLIER 65599
#define HASH_INIT_SIZE ((unsigned int)509)

struct SymTable_list_node{
	char* key;
	void* value;
	struct SymTable_list_node* next;	
	unsigned int hashed_at;
};

typedef struct SymTable_list_node Sym_list_T;

struct SymTable_array_cell{
	struct SymTable_list_node* head;
};

typedef struct SymTable_array_cell Sym_cell_T;

struct symtable_s{
	struct SymTable_array_cell *hash_table;
	unsigned int size;
	unsigned int capacity;
};





static unsigned int new_capacity_calc(unsigned int old_capacity){
	if(old_capacity == 509) return 1021; 
	if(old_capacity == 1021)return 2053; 
	if(old_capacity == 2053)return 4093; 
	if(old_capacity == 4093)return 8191; 
	if(old_capacity == 8191)return 16381; 
	if(old_capacity == 16381)return 32771; 
	if(old_capacity == 32771)return 65521; 

	assert(0);
	return 0;
}


static unsigned int SymTable_hash(const char *pcKey, unsigned int range);

static int hash_Table_put(Sym_cell_T* Hash_table, const char *pcKey, const void *pvValue, unsigned int  capacity){
	Sym_cell_T* table;
	Sym_list_T* list;
	Sym_list_T* new_node;
	unsigned int slot;

	assert(Hash_table != NULL);
	assert(pcKey != NULL);

	table = Hash_table;
	slot = SymTable_hash(pcKey, capacity);
	list = table[slot].head;

	new_node = malloc(sizeof(Sym_list_T));
	assert(new_node != NULL);

	/*creating copy of the key for stability*/
	new_node->key = malloc(strlen(pcKey) + 1);
	assert(new_node->key != NULL);
	strcpy(new_node->key, pcKey);

	/*we could make the value const in the struct
	 * but the we would to change some declarations
	 * ex the get function to return const void* */
	new_node->value = (void*)pvValue;
	
	new_node->next = list;
	table[slot].head = new_node;

	return 1;
}

static void Hash_table_free(Sym_cell_T* Hash_table, unsigned int capacity){
	Sym_cell_T* table;
	Sym_list_T* list;
	Sym_list_T* next_node;
	unsigned int i;

	if(Hash_table == NULL) return ;

	table = Hash_table;
	for(i = 0; i < capacity; i++){
		list = table[i].head;
		while(list != NULL){
			next_node = list->next;
			
			free(list->key);
			free(list);

			list = next_node;
		}
	}
	free(table);
	return ;
	
}


/*takes a key and a range and return the hashed slot*/
static unsigned int SymTable_hash(const char *pcKey, unsigned int range){
	size_t ui;
	unsigned int uiHash = 0U;

	for (ui = 0U; pcKey[ui] != '\0'; ui++)
		uiHash = uiHash * HASH_MULTIPLIER + pcKey[ui];
	return uiHash % range;
}

/*takes no arguments.
 * returns a new empty symtable with no bindings*/
SymTable_T SymTable_new(void){
	int i;
	struct symtable_s* new;
        
	new = malloc(sizeof(struct symtable_s));
	assert(new != NULL);

	new->hash_table = malloc(sizeof(Sym_cell_T) * HASH_INIT_SIZE);
	assert(new->hash_table != NULL);

	for (i = 0; i < HASH_INIT_SIZE; i++){
		new->hash_table[i].head = NULL;
	}
	new->size = 0;
	new->capacity = HASH_INIT_SIZE;

	return new;
}

/*takes the symtable to free
 * frees all the memory previously allocated
 * by the SymTable_ factions
 * if oSymTable == NULL the function does nothing*/
void SymTable_free(SymTable_T oSymTable){
	Sym_cell_T* table;
	Sym_list_T* list;
	Sym_list_T* next_node;
	unsigned int i;

	if(oSymTable == NULL) return ;

	table = oSymTable->hash_table;
	for(i = 0; i < oSymTable->capacity; i++){
		list = table[i].head;
		while(list != NULL){
			next_node = list->next;
			
			free(list->key);
			free(list);

			list = next_node;
		}
	}
	free(table);
	free(oSymTable);

	return ;
	
}

/*takes the Symtable of which to take the length 
 * returns the number of bindings that the  given table has
 * checks for runtime error if oSymTable == NULL */
unsigned int SymTable_getLength(SymTable_T oSymTable){
	Sym_cell_T* table;
	Sym_list_T* list;
	unsigned int lenght;
	unsigned int i;

	assert(oSymTable != NULL);

	table = oSymTable->hash_table;
	lenght = 0;
	for(i = 0; i < oSymTable->capacity; i++){
		list = table[i].head;
		while(list != NULL){lenght++; list = list->next;}
	}
	return lenght;
}


/*takes the Symtable in with to add the new binding
 * the key of the binding
 * the value of the binding
 *
 * if the key doesn't mach an existing binding
 * it crates a new binding with the given values
 * and returns 1
 * else if doesn't change the symtable and returns 0 
 * it checks for runtime error if oSymTable == NULL or pcKey == NULL*/
int SymTable_put(SymTable_T oSymTable, const char *pcKey, const void *pvValue){
	Sym_cell_T* table;
	Sym_list_T* list;
	Sym_list_T* new_node;
	unsigned int slot;

	unsigned int new_capacity; 
	unsigned int old_capacity;
	Sym_cell_T* new_hash_table;
	unsigned int i;

	assert(oSymTable != NULL);
	assert(pcKey != NULL);

	//if(SymTable_containsDuplicate(oSymTable, pcKey)) return 0;
	
	table = oSymTable->hash_table;
	slot = SymTable_hash(pcKey, oSymTable->capacity);
	list = table[slot].head;

	new_node = malloc(sizeof(Sym_list_T));
	assert(new_node != NULL);

	/*creating copy of the key for stability*/
	new_node->key = malloc(strlen(pcKey) + 1);
	assert(new_node->key != NULL);
	strcpy(new_node->key, pcKey);

	/*we could make the value const in the struct
	 * but the we would to change some declarations
	 * ex the get function to return const void* */
	new_node->value = (void*)pvValue;
	
	new_node->hashed_at = oSymTable->capacity;
	oSymTable->size += 1;
	
	new_node->next = list;
	table[slot].head = new_node;

 	if( (oSymTable->size >= oSymTable->capacity) && oSymTable->capacity < 65521){
		old_capacity = oSymTable->capacity;
		new_capacity = new_capacity_calc(old_capacity);
		 
		new_hash_table = malloc(sizeof(Sym_cell_T) * new_capacity);
//printf("\n===================================================\nrehashing trigered:\nold address %p with capacity %d\nnew address %p with capacity %d\n", (void *)oSymTable->hash_table, old_capacity, (void *)new_hash_table, new_capacity);
		/*init the new table*/
		for(i = 0; i < new_capacity; i++){
			new_hash_table[i].head = NULL;
		}
	
		/*rehash all binding form the old to the new table*/
		for(i = 0; i < old_capacity; i++){
			list = oSymTable->hash_table[i].head;
			while(list != NULL){;
				hash_Table_put(new_hash_table, list->key, list->value, new_capacity);
				list = list->next;
			}	
		}
		Hash_table_free(oSymTable->hash_table, old_capacity);
		oSymTable->hash_table = new_hash_table;		
		oSymTable->capacity = new_capacity;
	}

	return 1;
}

/*takes a symtable from which to revove the binding(oSymTable)
 * the key of the binding to revove
 * if the there is a binding with the given key it 
 * removes  the binding and returns 1
 * else it doesn't change the table and returns 0 
 * it checks for runtime error if oSymTable == NULL
 * or pcKey == NULL  */
int SymTable_remove(SymTable_T oSymTable, const char *pcKey){
	Sym_cell_T* table;
	Sym_list_T* list;
	Sym_list_T* prev;
  	unsigned int slot;

	assert(oSymTable != NULL);
	assert(pcKey != NULL);

	if( !(SymTable_contains(oSymTable, pcKey) ) )  return 0;

	table = oSymTable->hash_table;
	slot = SymTable_hash(pcKey, oSymTable->capacity);
	list = table[slot].head;
	
	/*if ist's the first node(using prev as next to avoid 
	 * new declaration*/
	if(strcmp(list->key, pcKey) == 0){
		prev = list->next;
		free(list->key);
		free(list);
		table[slot].head = prev;
		return 1;
	}
	/*else*/
	while (list != NULL && strcmp(list->key, pcKey) != 0){
			prev = list;
			list = list->next;
	}
	
	if(list != NULL){
		prev->next = list->next;
		free(list->key);
		free(list);
	}else assert(0);
	
	return 1;
	
}

/*takes the symtable to check if it contains a binding(oSymTable)
 *the key to check if exists(pcKey)
 *
 *it returns 1 if the binding exists else return 0  
 *
 * it checks for runtime error if oSymTable == NULL
 * or pcKey == NULL  */
int SymTable_contains(SymTable_T oSymTable, const char *pcKey){
	Sym_cell_T* table;
	Sym_list_T* list;
	int i;

	assert(oSymTable != NULL);	
	assert(pcKey != NULL);

	table = oSymTable->hash_table;
	for(i = 0; i < oSymTable->capacity; i++){
		list = table[i].head;
		while(list != NULL){
			if( (strcmp(list->key, pcKey) == 0) && ( ((SymbolTableEntry_t *)list->value)->isActive == 1) )return 1; 
			
			list = list->next;
		}
	}
	return 0;
}

/*takes the symtable form which to get a value(oSymTable)
 *the key from which to get the value (pcKey)
 *
 *if the binding exits it return the value of the binding 
 *else it return NULL
 *
 * it checks for runtime error if oSymTable == NULL
 * or pcKey == NULL  */
void *SymTable_get(SymTable_T oSymTable, const char *pcKey){
	Sym_cell_T* table;
	Sym_list_T* list;
  	unsigned int slot;

	assert(oSymTable != NULL);
	assert(pcKey != NULL);

	table = oSymTable->hash_table;
	slot = SymTable_hash(pcKey, oSymTable->capacity);
	list = table[slot].head;
	
	while(list != NULL){
		if( (strcmp(list->key, pcKey) == 0) && ( ((SymbolTableEntry_t *)list->value)->isActive == 1) )return list->value; 
		
		list = list->next;
	}	
	return NULL;
}

/*takes the symtable to aply the function at its bindings(oSymTable)
 * the function to apply(pfApply) 
 * and an extra parameter to pass on the pfApply (pvExtra) 
 * 
 * it applies the giver function with each key and value and the pvExtra
 * it checks for runtime if oSymTable == NULL or pfApply == NULL */
void SymTable_map(SymTable_T oSymTable, void (*pfApply)(const char *pcKey, void *pvValue, void *pvExtra),const void *pvExtra){
	Sym_cell_T* table;
	Sym_list_T* list;
	int i;

	assert(oSymTable != NULL);
	assert(pfApply != NULL);

	table = oSymTable->hash_table;
	for(i = 0; i < oSymTable->capacity; i++){
		list = table[i].head;
		while(list != NULL){	
			/*it might be better to change the parameter of pfApply to const void* to 
			avoid the cast*/
			pfApply(list->key, list->value, (void*) pvExtra); 
		       	
			list = list->next;
		}
	}
	return ;
}
