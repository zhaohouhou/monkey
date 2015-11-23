//Used to generate the linked list defining code(ML). 
//Run with the list length number.

#include <stdio.h>
#include <time.h>
#include <stdlib.h>

FILE *out;

void genList(int n){
	int v;
	if(n==0){
		fprintf(out, "Nil ()");
		return;
	}
	v = (rand() % 1000) -400; 
	if(v>=0)
		fprintf(out, "Some (%d, ", v);
	else
		fprintf(out, "Some (~%d, ", 0-v);
	genList(n-1);
	fprintf(out, ") ");
}

int main (int argc,char *argv[])
{
  int i, n;
  srand((unsigned) time(NULL));
  out = fopen("out-list.txt", "w+");	
  for(i = 0, n = 0; argv[1][i]!='\0'; i++){
	  n+= argv[1][i] - '0';
	  n *= 10;
  }
  n /= 10;
  genList(n);
  return 0;
}

