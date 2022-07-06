import { Injectable } from '@angular/core';

var storage = window.localStorage;

@Injectable()
export class LocalstorageService {

  constructor() {}

  public setData(name:string, value:string):void {
    storage.setItem(name, value);
  }

  public getData(name:string):string {
    return storage.getItem(name);
  }

  public setJSONData(name:string, value:any):void {
    let val = JSON.stringify(value);
    storage.setItem(name, val);
  }

  public getJSONData(name:string):any {
    let val = storage.getItem(name);
    return JSON.parse(val);
  }

}
