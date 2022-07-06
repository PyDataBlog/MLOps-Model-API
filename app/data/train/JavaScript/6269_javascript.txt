(function () {
    'use strict';
    function Cat(name, color) {
        this.name = name;
        this.color = color;
    }
    Cat.prototype.age = 4;
    var huesitos = new Cat('huesitos', 'amarillo claro');
    display(huesitos.__proto__);
    display(Cat.prototype );
    display(huesitos.__proto__ == Cat.prototype );

    Cat.prototype = { age: 5};
    display(Cat.prototype);
    
} ());