import { Component, OnInit } from '@angular/core';
import {HeroesService, Hero} from '../../services/heroes.services';

@Component({
  selector: 'app-search',
  templateUrl: './search.component.html'
})
export class SearchComponent implements OnInit {
  heroesSearch:any[] = [];
  constructor(private _heroesService: HeroesService) { }

  ngOnInit() {
  }

  searchHero(text:string) {
    console.log(text);
    // this.heroesSearch = this._heroesService.getHeroes(text);
  }
}
