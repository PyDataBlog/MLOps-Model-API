#include <iostream>
using namespace std;
bool touched[103];
int printSearch(int *arr,int j,int n,int ele){
    int i;
    for(i=j+1;(i<=n && arr[i]!=ele);i++);
    touched[i]=1;
    return i;
}
int main(int argc, char const *argv[]) {
    int n,arr[103],sum=0;
    cin>>n;
    for(int i=1;i<=n;i++) {cin>>arr[i];sum+=arr[i];}
    sum/=(n>>1);
    for(int i=1;i<=(n);i++){
        if(touched[i]) continue;
        cout<<i<<" "<<printSearch(arr,i,n,sum-arr[i])<<endl;
    }
    return 0;
}
