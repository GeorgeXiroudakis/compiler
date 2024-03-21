#include "definitions.h"


typedef struct symtable_s* SymTable_T;

struct symtable_s;

/*takes no arguments.
 * returns a new empty symtable with no bindings*/
SymTable_T SymTable_new(void);

/*takes the symtable to free
 * frees all the memory previously allocated
 * by the SymTable_ factions
 * if oSymTable == NULL the function does nothing*/
void SymTable_free(SymTable_T oSymTable);

/*takes the Symtable of which to take the length 
 * returns the number of bindings that the  given table has
 * checks for runtime error if oSymTable == NULL */
unsigned int SymTable_getLength(SymTable_T oSymTable);

/*takes the Symtable in with to add the new binding
 * the key of the binding
 * the value of the binding
 *
 * if the key doesn't mach an existing binding
 * it crates a new binding with the given values
 * and returns 1
 * else if doesn't change the symtable and returns 0  
 * it checks for runtime error if oSymTable == NULL or pcKey == NULL*/
int SymTable_put(SymTable_T oSymTable, const char *pcKey, const void *pvValue);

/*takes a symtable from which to revove the binding(oSymTable)
 * the key of the binding to revove
 * if the there is a binding with the given key it 
 * removes  the binding and returns 1
 * else it doesn't change the table and returns 0  
 * it checks for runtime error if oSymTable == NULL
 * or pcKey == NULL  */
int SymTable_remove(SymTable_T oSymTable, const char *pcKey);

/*takes the symtable to check if it contains a binding(oSymTable)
 *the key to check if exists(pcKey)
 *
 *it returns 1 if the binding exists else return 0  
 *
 * it checks for runtime error if oSymTable == NULL
 * or pcKey == NULL  */
int SymTable_contains(SymTable_T oSymTable, const char *pcKey);

/*takes the symtable form which to get a value(oSymTable)
 *the key from which to get the value (pcKey)
 *
 *if the binding exits it return the value of the binding 
 *else it return NULL
 *
 * it checks for runtime error if oSymTable == NULL
 * or pcKey == NULL  */
void *SymTable_get(SymTable_T oSymTable, const char *pcKey);

/*takes the symtable to aply the function at its bindings(oSymTable)
 * the function to apply(pfApply) 
 * and an extra parameter to pass on the pfApply (pvExtra) 
 * 
 * it applies the giver function with each key and value and the pvExtra
 * it checks for runtime if oSymTable == NULL or pfApply == NULL */
void SymTable_map(SymTable_T oSymTable, void (*pfApply)(const char *pcKey, void *pvValue, void *pvExtra),const void *pvExtra);
