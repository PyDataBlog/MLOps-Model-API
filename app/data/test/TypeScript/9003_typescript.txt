function sumar(x: number, y: number): number {
    return x + y;
}

console.log(sumar(1, 1));

let f1 = function (x: number, y: number): number {
    return x + y;
}

console.log(f1(1, 1));

let f2: (a: number, b: number) => number = 
function (x: number, y: number): number {
    return x + y;
}

console.log(f2(1, 1));

let f3: (a: number, b: number) => number 
= (x, y) => x + y;

console.log(f3(1, 1));

let f4 = (x, y) => x + y;

console.log(f4(1, 1));
console.log(f4('a', 'b'));
