import { Injectable } from '@angular/core';
import { Headers, Http, Response } from '@angular/http';


import { Floss } from '../models/floss';
import { DB } from '../models/db';

@Injectable()
export class CatalogService {
  private flossUrl = 'app/floss';  // URL to web api

  constructor(private http: Http) { }

  getCatalog(): Promise<Floss[]> {
    return Promise.resolve(DB.db);
  }

  getFloss(id: number): Promise<Floss> {
    return this.getCatalog()
      .then(catalog => catalog.find(floss => floss.dmc === id));
  }

  deleteFloss(floss: Floss, group : string): void {
    var my : Floss[] = this.load(group);
    if(!my){
      my = [];
    }
    let newArr : Floss[] = my.filter(value => value.dmc != floss.dmc);
    console.log(newArr);
    this.store(group, newArr);
  }

  addFlossTo(floss: Floss, group: string) : void{
    var collection : Floss[] = this.load(group);
    if(!collection){
      collection = [];
    }
    let found : Floss = collection.find(value => value.dmc === floss.dmc);
    if(found)
      return;
    console.log(collection);
    collection.push(floss);
    this.store(group, collection);
  }

  isFlossIn(floss: Floss, group: string) : boolean {
    var collection : Floss[] = this.load(group);
    if(!collection){
      collection = [];
    }
    let found : Floss = collection.find(value => value.dmc === floss.dmc);
    return found ? true : false; 
  }

  store(name: string, data: any): void {
    let localData: any = localStorage.getItem('sara');
    if (localData) {
      localData = JSON.parse(localData);
    } else {
      localData = {};
    }

    localData[name] = data;

    localStorage.setItem('sara', JSON.stringify(localData))
  }

  load(name: string): any {
    let data: any = JSON.parse(localStorage.getItem('sara'));
    if (!data) {
      return undefined;
    }
    if (name) {
      if (data[name]) {
        return data[name];
      } else {
        return undefined;
      }
    }
    return data;
  }

}
