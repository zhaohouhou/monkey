//Used to generate nested functions (in ML). 
//Run with the number of nesting layers.

#include <stdio.h>
#include <stdlib.h>

FILE *out;

void genFunc(int n){
	if(n==1){
		fprintf(out, "fun F1 X1 = X1+1 ");
		return;
	}
	fprintf(out, "fun F%d X%d = \nlet ", n, n);
	genFunc(n-1);
	fprintf(out, "in F%d (X%d +1)\nend\n ", n-1, n);
}

int main (int argc,char *argv[])
{
  int i, n;
  srand((unsigned) time(NULL));
  out = fopen("out-func.txt", "w+");	
  for(i = 0, n = 0; argv[1][i]!='\0'; i++){
	  n+= argv[1][i] - '0';
	  n *= 10;
  }
  n /= 10;
  fprintf(out, "val code = let \n");
  genFunc(n);
  fprintf(out, "in print (int2string(F%d 1))\nend\n", n);
  return 0;
}

