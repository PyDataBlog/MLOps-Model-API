function pyramid (a:number):void{
    for(var  i:number = 1; i <= a; i++){
        var b:string = '';
        for(var j:number = 0; j <= a-i-1 ; j++){
            b += ' ';
        }
        for(var k:number = 1; k <= 2*i-1; k++){
            b += '*';
        }
        console.log(b);
    }
}
//var numS:string = prompt("请输入一个数字","");
//var num:number = +numS;

var numS:string = process.argv[2];
//var num:number = parseInt(numS);
pyramid(parseInt(numS));