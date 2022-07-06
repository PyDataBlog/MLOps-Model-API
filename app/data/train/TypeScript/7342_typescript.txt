var i:number =0;
var j:number=0;
var n:number=3;
var line:number=0;

var a="";
var b="";
var c="";
var d="*";
line =getLine(n);
star(line);
function star(n:number):
void{
    for(i=0;i<n;i++){
        for(j=0;j<n-i;j++){
            a=a+c;
        }
        for(j=0;j<2*i+1;j++){
            b=b+d;
        }
        console.log(a+b);
        a=""
        b=""
    }
}
function getLine(line:number):number{
    return line;
}