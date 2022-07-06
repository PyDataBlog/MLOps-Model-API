import { Injectable } from 'angular2/core'
import { Http, Response } from 'angular2/http';
import { Beer } from './beer'
import 'rxjs/Rx';

@Injectable()
export class BeerService {
  private _breweryDbKey = '2d18b2531035b441a50dddc3aed32a1b';
  private _beerUrl = "http://localhost:1337/api.brewerydb.com/v2/beers?availableId=1&withBreweries=Y&key=" + this._breweryDbKey;
  private _searchUrl = "http://localhost:1337/api.brewerydb.com/v2/search?type=beer&withBreweries=Y&key=" + this._breweryDbKey + '&q=';

  constructor(private http: Http) {}
  getBeer() {
    return this.http.get(this._beerUrl)
      .map((response: Response) => {
        return response.json().data.map(item => {
          return new Beer({
            name: item.nameDisplay,
            brewery: item.breweries[0].nameShortDisplay,
            description: item.description,
            abv: item.abv,
            ibu: item.ibu,
            type: item.style.shortName
          });
        });
      })
      .catch((err: Response) => console.log(err));
  }
  search(term: string) {
    return this.http.get(this._searchUrl + term)
      .map((response: Response) => {
        return response.json().data.map(item => {
          return new Beer({
            name: item.nameDisplay,
            brewery: item.breweries[0].nameShortDisplay,
            description: item.description,
            abv: item.abv,
            ibu: item.ibu,
            type: item.style.shortName
          });
        })
      })
      .catch((err: Response) => console.log(err));
  }
}