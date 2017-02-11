#include<stdio.h>
char mat[3][3]={' ',' ',' ',
' ',' ',' ',
' ',' ',' '},i,arr[9]={0,0,0,0,0,0,0,0,0};	//arr is 0 where input is to be taken

void printmat(){
	int i,j;
	printf("\n");
	for(i=0;i<3;i++){
	for(j=0;j<3;j++)
		printf("%c ",mat[i][j]);
		printf("\n");
		}
}

void printopt(){
	printf("\n");
	int i,j,p,c=1,t;
	for(i=0;i<3;i++)
	for(j=0;j<3;j++){
		//printf("%d",mat[i][j]);
		if(mat[i][j]!=' '){
			t=3*i+j;
			arr[t]=1;
		}
	}

	for(p=0;p<9;p++){
		if(arr[p]==0){
		printf("%d.(%d,%d)\n",c,(p)/3+1,(p)%3+1);
		c++;
		}
	}
}

int matammend(int ind,int in){
	int i;
	for(i=0;i<9;i++){
		if(arr[i]==0){
			in--;
			if(in==0){
				if(ind%2==0)mat[i/3][i%3]='X';
				else mat[i/3][i%3]='O';
			}
		}
	}
}
int main(){
int in;
for(i=0;i<9;i++){

printopt();
scanf("%d",&in);
matammend(i,in);
printmat();


}
	
}

