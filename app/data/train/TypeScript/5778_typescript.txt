// Add decorator notify. 
// Decorator before invoke the method should be invoke all methods 
// from input array of callbacks.

function notifyDecorator(target, propertyKey: string, descriptor: PropertyDescriptor): any {
	descriptor.value = function(arg) :void{
		for (var i = arg.length - 1; i >= 0; i--) {
			arg[i]();
		}
	};
}

function callback1(){
	console.log('Callback 1')
}

function callback2(){
	console.log('Callback 2')
}

function callback3(){
	console.log('Callback 3')
}

function callback4(){
	console.log('Callback 4')
}

class classToTest {
	@notifyDecorator
	someMethod(args){
		return args;
	}
}

let someTestClass = new classToTest();
someTestClass.someMethod([callback1,callback2,callback3,callback4])
