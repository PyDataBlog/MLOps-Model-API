import {Injectable} from '@angular/core';

@Injectable()
export class PreloaderService {

  private static loaders:Array<Promise<any>> = [];

  public static registerLoader(method:Promise<any>):void {
    PreloaderService.loaders.push(method);
  }

  public static clear():void {
    PreloaderService.loaders = [];
  }

  public static load():Promise<any> {
    return new Promise((resolve, reject) => {
      PreloaderService.executeAll(resolve);
    });
  }

  private static executeAll(done:Function):void {
    setTimeout(() => {
      Promise.all(PreloaderService.loaders).then((values) => {
        done.call(null, values);

      }).catch((error) => {
        console.error(error);
      });
    });
  }
}
