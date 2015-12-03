#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

char* int2string (int i)
{
  char* a=malloc(10*sizeof(char));
  char* p = a+9;
  int neg = 0;
  *p = '\0';
  p--;
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



int print (const char *s)
{
  return printf ("%s", s);
}

void * allocTuple(int length, ...){
  va_list valist;  int i;  int * p;
  p = (int * )malloc(sizeof(void * )*(length + 1));
  va_start(valist, length);
  for (i = 1; i <= length; i++){
    p[i] = va_arg(valist, int);
  }
  p[0] = 1;
  return p;
}

void * allocTag(int tag, void * x){
  int * p;
  p = (int * )malloc(sizeof(void * )*2);
  p[0] = tag;
  p[1] = (int)x;
  return p;
}

