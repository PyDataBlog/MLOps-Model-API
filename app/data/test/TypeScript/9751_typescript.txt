/*  Implement override method that can retrieve day, month(as number as text format) and year and return Date object
    Override this method for retrieve array of such parameters and retrieve array with dates. */

// SZ classes, enums and interfaces should be implemented in separete file.
interface ICustomDate {
    day: number;
    month: number | string;
    year: number;
}

// SZ At this course each property of enum should has a value.
enum monthes {
    january, february, march, april, may, june, july, august, september, october, november, december
} 

function over(day: Array<ICustomDate>): Array<Date>;
function over(day: number, month: number | string, year: number): Date;

function over(day: number | Array<ICustomDate>, month?: number | string, year?: number): Array<Date> | Date {
    let d: Array<Date> | Date;
    if(typeof day === 'object') { // it isn't day
        d = [];
        for(let i = 0; i < day.length; i++) {
            if(typeof day[i].month === 'number') {
                d.push(new Date(day[i].year, Number(day[i].month), day[i].day));
            }
            if(typeof day[i].month === 'string') {
                let temp: string = String(day[i].month);
                d.push(new Date(day[i].year, monthes[temp.toLowerCase()], day[i].day));
            }
        }
    }
    if(typeof day === 'number') {
        if(typeof month === 'number') {
            d = new Date(year, month, day);
        }
        if(typeof month === 'string') {
            d = new Date(year, monthes[month.toLowerCase()], day);
        }
    }

    return d;
}

let first_call: Date = over(1, 1, 1);
console.log(first_call);

let second_call: Date = over(12, 'June', 1091);
console.log(second_call);

let cdate: Array<ICustomDate> = [];
let tempdate: ICustomDate = { day: 1, month: 'September', year: 1939};
cdate.push(tempdate);
cdate.push({ day: 2, month: 8, year: 1945 });

let third_call: Array<Date> = over(cdate);

for(let i = 0; i < third_call.length; i++) {
    console.log(third_call[i]);
}

/*  Implements to string method. Retrieve different type of object returns string */

function getPrimitiveS(primitive: number | string | boolean | null | undefined): string { // also 'function'
    if(typeof primitive === 'undefined') {
        return 'undefined';
    }
    if(primitive === null) {
        return 'null';
    }
    return primitive.toString();
}

function customToString(obj: object): string {
    let result: string = '';
    if (typeof obj === 'object' && obj !== null) {
        for(let key in obj) {
            result += `${key}: `
            if (typeof obj[key] === 'object' && obj[key] !== null) {
                result += `{`
                result += customToString(obj[key]);
                result += '}, ';
            } else {
                result += getPrimitiveS(obj[key]);
                result += ', ';
            }
        }
    } else {
        result = 'null';
        result += ', ';
    }
    return result;
}

// check
console.log('-------');

let someObject: object = {
    sleep: false,
    shave: (): void => {
        let cheeck = 'sleek';
    },
    wisdom: {
        forehead: 7,
        humour: 'black',
        incontinence: undefined,
        test: null
    }
};

console.log(customToString(someObject));

class A {
    public a: number;
    private b: string;
    protected c: boolean;

    constructor(a: number, b:string, c: boolean) {
        this.a = a;
        this.b = b;
        this.c = c;
    }
}

console.log(customToString(A));

let a = new A(1, '2', true);

console.log(customToString(a));

/*  Implements valueOf method. Retrieve different type of objects returns true/false. */

function getPrimitiveV(primitive: number | string | boolean | null | undefined): number { // also 'function'
    if(typeof primitive === 'number' || typeof primitive === 'boolean') {
        return Number(primitive.valueOf());
    }
    if(typeof primitive === 'string') {
        if(!isNaN(Number(primitive.valueOf()))) {
            return Number(primitive.valueOf());
        } else {
            return 1;
        }
    }
    if(typeof primitive === 'undefined') {
        return 0;
    }
    if(primitive === null) {
        return 0;
    }
    if(typeof primitive === 'function') {
        return 1;
    }
}

function customToValue(obj: object): number {
    let result: number = 0;
    if (typeof obj === 'object' && obj !== null) {
        for(let key in obj) {
            if (typeof obj[key] === 'object') {
                result += customToValue(obj[key]);
            } else {
                result += getPrimitiveV(obj[key]);
            }
        }
    } else {
        result = 0;
    }
    return result;
}

// check
console.log('-------');
console.log(customToValue(someObject));

console.log(customToValue(A));

console.log(customToValue(a));
