import { Injectable } from "@angular/core";

@Injectable()

export class DemoService {
    sayHello(name:string) {
        return "Hello " + name;
    }
}