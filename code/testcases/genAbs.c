//Used to generate nested abstractions (in ML). 
//Run with the number of nesting layers.

#include <stdio.h>
#include <stdlib.h>

FILE *out;

int N;

void genFunc(int n){
	int i;
	if(n==1){
		fprintf(out, "let val X%d = fn Y%d =>\n", n, n);
		fprintf(out, "Y1 ");
		for(i = 2; i<= N; i++){
			fprintf(out, "+ Y%d", i);
		}
		fprintf(out, "\nin X%d\nend\n ", n);
		return;
	}
	
	fprintf(out, "let val X%d = fn Y%d =>\n", n, n);
	genFunc(n-1);
	fprintf(out, "\nin X%d\nend\n ", n);
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
  fprintf(out, "val code = let val F = \n");
  N = n;
  genFunc(n);
  fprintf(out, "in print (int2string( F");
  for(i = 1; i<= N; i++){
			fprintf(out, " %d", i);
		}
  fprintf(out, "))\nend\n", n);
  return 0;
}

