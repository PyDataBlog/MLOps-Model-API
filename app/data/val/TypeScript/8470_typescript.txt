import {Coffee} from './Coffee';

export class MilkCoffee implements Coffee {
  protected coffee:Coffee;

  public constructor(coffee:Coffee) {
    this.coffee = coffee;
  }

  public getCost():number {
    return this.coffee.getCost() + 2;
  }

  public getDescription():string {
    return `${this.coffee.getDescription()}, milk`;
  }
}
