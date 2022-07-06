import chance = require('../../extlibs/wrap_chance');

export class Question {
    
    num: number;

    constructor() {
        this.num = chance.natural({ min: 20, max: 20000 });
    }

    get avg() {
        return this.num + 0.5;
    }

    get cans() {
        return this.num + 2;
    }

    get qtxt() {
        return `Average of 4 consecutive numbers is ${this.avg}, largest number is `
    }
}
