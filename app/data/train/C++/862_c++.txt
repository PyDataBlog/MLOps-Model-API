#include<iostream>
using namespace std;
int n,k;
const int maxn=100010;
bool b[2*maxn];
struct
{
    int num,cn;
    
}queue[2*maxn];
int head,tail;

void bfs()
{
    b[n]=1;
    queue[0].num=n;
    queue[0].cn=1;
    while(head<=tail)
    {
        if(queue[head].num==k)
        {
            printf("%d\n",queue[head].cn);
            return;
        }
        else if(queue[head].num>k&&!b[queue[head].num-1])
        {
            b[queue[head].num-1]=1;
            queue[++tail].num=queue[head].num-1;
            queue[tail].cn=queue[head].cn+1;
        }
        else
        {
            if(queue[head].num>0&&!b[queue[head].num-1])
            {
                b[queue[head].num-1]=1;
                queue[++tail].num=queue[head].num-1;
                queue[tail].cn=queue[head].cn+1;
            }
            
            if(!b[queue[head].num*2])
            {
                b[queue[head].num*2]=1;
                queue[++tail].num=queue[head].num*2;
                queue[tail].cn=queue[head].cn+1;
            }
            
            if(!b[queue[head].num+1])
            {
                b[queue[head].num+1]=1;
                queue[++tail].num=queue[head].num+1;
                queue[tail].cn=queue[head].cn+1;
            }
        }
        ++head;
    }
}
int main()
{
    scanf("%d%d",&n,&k);
    
    if(n==k)
    {
        printf("1\n");
        return 0;
    }

    memset(b,0,sizeof(b));
    
    head=tail=0;
    bfs();
    return 0;
}
