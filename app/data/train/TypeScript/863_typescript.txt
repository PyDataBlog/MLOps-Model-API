export class Animal {
    protected name: string;

    constructor(name: string) {
        this.name = name;
    }

    public say() {
        this.out(this.name);
    }

    protected out(str: string) {
        console.info(str);
    }
}
