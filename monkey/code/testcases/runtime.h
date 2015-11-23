#ifndef RUNTIME_H
#define RUNTIME_H

int ml_add (int i, int j);
int ml_sub (int i, int j);
int ml_lessThan (int i, int j);
int ml_largerThan (int i, int j);
int ml_times (int i, int j);
int ml_print (const char *);
char *ml_int2string (int i);

// GC-related
void *Gc_allocTuple (int);
void *Gc_allocTag (int);
void Gc_init(int size);
void Gc_retrieve(int need);

//map: function -> size
void Gc_newMap();
void Gc_insertMap(void *funcptr, int size);
int Gc_lookupMap(void *func);

#endif
