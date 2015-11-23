//Used to generate the binary tree defining code(ML). 
//Run with the node number.

#include <stdio.h>
#include <time.h>
#include <stdlib.h>

FILE *out;

void genTree(int n){
	int v;
	if(n==0){
		fprintf(out, "Nil ()");
		return;
	}
	v = rand() % 1000; 
	fprintf(out, "Node (%d, ", v);
	genTree(n/2);
	fprintf(out, ", ");
	genTree(n/2);
	fprintf(out, ") ");
}

int main (int argc,char *argv[])
{
  int i, n;
  srand((unsigned) time(NULL));
  out = fopen("out-tree.txt", "w+");	
  for(i = 0, n = 0; argv[1][i]!='\0'; i++){
	  n+= argv[1][i] - '0';
	  n *= 10;
  }
  n /= 10;
  genTree(n);
  return 0;
}

