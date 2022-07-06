import {Injectable} from '@angular/core';

//import {HEROES} from './mock-heroes';
import {Http,Headers} from '@angular/http';
import 'rxjs/add/operator/toPromise';

import {Hero} from './model/Hero';



@Injectable()
export class HeroService  {
	
	private hero:Hero;
	private heroesApiUrl = 'api/heroes'; //URL to web api
	private headers = new Headers({'Content-Type': 'application/json'});
	constructor(private http:Http){

	}

	getHeroes():Promise<Hero[]>{
		//return  Promise.resolve(HEROES);
		return  this.http.get(this.heroesApiUrl)
			.toPromise()
			.then(response => response.json().data as Hero[])
			.catch(this.handleError);
	}

	getHeroesSlowly() {
		//return new Promise<Hero[]>(resolve =>
    	//setTimeout(() => resolve(HEROES), 4000)); // 4 seconds 
	}

	getHero(heroId: number | string) {
	  	return this.getHeroes()
               .then(heroes => heroes.find(hero => hero.id === heroId));
	}

	update(hero: Hero): Promise<Hero> {
		const url = `${this.heroesApiUrl}/${hero.id}`;
	  	return this.http
		    .put(url, JSON.stringify(hero), {headers: this.headers})
		    .toPromise()
		    .then(() => hero)
		    .catch(this.handleError);
	}

	private handleError(error: any): Promise<any> {
  		console.error('An error occurred', error); // for demo purposes only
  		return Promise.reject(error.message || error);
	}
	create(name:string):Promise<Hero>{
		return this.http
		    .post(this.heroesApiUrl, JSON.stringify({name: name}), {headers: this.headers})
		    .toPromise()
		    .then(res => res.json().data)
		    .catch(this.handleError);
	}
}