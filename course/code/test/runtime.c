#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

int ml_add (int i, int j)
{
  return (((i>>1)+(j>>1))<<1)|1;
}

int ml_sub (int i, int j)
{
  return (((i>>1)-(j>>1))<<1)|1;
}

int ml_times (int i, int j)
{
  return (((i>>1)*(j>>1))<<1)|1;
}

char* ml_int2string (int i)
{
  char* a=malloc(10*sizeof(char));
  char* p = a+9;
  int neg = 0;
  *p = '\0';
  p--;
  i = i>>1;
  if(i==0) {
    *p = '0';
    return p;
  }
  if(i<0) {
    neg = 1;
    i = 0 -i;
  }
  while(i>0){
    *p = '0'+ (i%10);
    p--;
    i = i/10;
  }
  if (neg){
    *p= '-';
    return p;
  }
  p++;
  return p;
}

int String2Int(char * s){
  int ans = 0;
  while(*s != '\0'){
    ans += (*s - '0');
    ans *= 10;
    s++;
  }
  ans /= 10;
  return ans;
}

int ml_print (const char *s)
{
  return printf ("%s", s);
}

//gc-related
void * heap;
int heap_size; //half heap size
int *to, *from;
int *to_end,*from_end;
extern int *_arg;

void *Gc_allocTuple (int numFields)
{
  int *p = from_end;
  from_end += (numFields+2);   //forwarding + size
  int i;
  for (i=0; i<numFields+2; i++)
    p[i] = 0;

  p[1] = (numFields)<<1;
  return p+1;
}

void *Gc_allocTag (int tag)
{
  int *p = from_end;
  from_end += 3;   //forwarding + tag + value
  int i;
  for (i=0; i<3; i++)
    p[i] = 0;

  p[1] = ((tag)<<1)|1;
  return p+1;
}

void Gc_init(int size){
  heap = malloc(size * sizeof(int) * 2);
  heap_size = size;
  from = from_end = heap;
  to = to_end = from + size;
}

void *move(int *old){
  //Move a structure from "from" zone to "to" zone.
  //Recursively move its components if necessary.
  //Set the "forwarding" zone to the new one.
  //Return the new pointer to the structure.

  assert(old != 0);
  int numFields;
  if(((*old)%2)==0)  //tuple
    numFields = (*old)>>1;
  else  //tag
    numFields = 1;
  int *new = to_end;
  to_end += (numFields+2);
  *new = 0; //forwarding = 0 for new sturcture.
  new ++;
  memcpy(new, old, (numFields+1)*sizeof(int));
  int *p = new;
  int *forward;
  while(numFields > 0){
    p ++;
    if(((*p)%2==0)&&((*p) < from_end)&&((*p) >= from)){
      //this field is a structure in heap.
      forward = (int *)(*p)-1; 
      if((*forward) !=0){
        assert(((*forward) < to_end)&&((*forward) >= to));
        *p = *forward;
      }
      else 
        *p = move(*p);
    }
    numFields --;
  }
  *(old -1) = new; //set forwarding
  return new;
}

void Gc_retrieve(int need){
  // printf("current space:%d, need: %d\n", heap_size - (from_end - from), need);
  if(heap_size - (from_end - from) > need)
    return; //enouph
  printf("gc...");
  if(_arg != 0)
    _arg = move(_arg);

  printf("freed %d bytes\n", (from_end-from)-(to_end-to));
  //exchange
  int * temp = from;
  from = to;
  to = temp;
  from_end = to_end;
  to_end = to;

  if(heap_size - (from_end - from) < need){
    printf("\nout of memory!\n");
    exit(0);
  }
}

//table mapping function to its size
struct map{
  void * funcptr;
  int size;
  struct map * next;
};

struct map *head, * rear;

void Gc_newMap(){
  head = rear = 0;
}

void Gc_insertMap(void *funcptr, int size){
  struct map* newmap = malloc(sizeof(struct map));
  newmap->funcptr = funcptr;
  newmap->size = size;
  newmap->next = 0;
  if(rear == 0){
    head = rear = newmap;
    return;
  }
  rear->next = newmap;
  rear = rear->next;
}

int Gc_lookupMap(void *func){
  struct map * p = head;
  while(p!=0){
    if(p->funcptr == func)
      return p->size;
    p = p->next;
  }
  printf("0x%x not found in map", func);;
  exit(0);
}
