##Generators

<div class="code-extra es6">
```js
// Define generators with '*'
function *foo() {	}

// halt and output at 'yield'
function *fibonacci() {
	let pre = 0, cur = 1;
	while(true) {
		[ pre, cur ] = [ cur, pre + cur ];
		yield cur;
	}
}

let fibGen = fibonacci();
fibGen.next(); // { value: 1, done: false }
fibGen.next(); // { value: 2, done: false }

// Can use For..Of on generators
for(let x of fibGen) {
  if (x > 1000) {
    break;
  }
  console.log(x);
}
// 1, 2, 3, 5, 8...
```
Note:
- defined with 'star'

- generators can be halted

- yield outputs value
- yield accepts value
