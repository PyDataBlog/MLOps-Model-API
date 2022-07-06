#include <iostream>
#include <string>
using namespace std;

int main(){
  int n;
  string experiment;
  for(cin>>n; n>0; n--){
    cin >> experiment;
    if (experiment.size() < 3) cout << '+' << endl;
    else{
      string s = experiment.substr(experiment.size()-2, 2);
      string saux;
      if(s == "35") cout << '-' << endl;
      else{
        s = s[1];
        saux = experiment[0];
        if(saux == "9" && s == "4") cout << '*' << endl;
        else{
          s = experiment.substr(0,3);
          if(s == "190") cout << '?' << endl;
          else cout << '?' << endl;
        }
      }
    }
  }



return 0;
}
