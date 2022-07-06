import { Component, OnInit } from '@angular/core';
import { RouteSegment } from '@angular/router';
import { SharedService } from '../../app/shared';
import { PokemonDetailsService } from './service/pokemon-details.service';
import { KeyValues} from '../../app/shared';

@Component({
  moduleId: module.id,
  selector: 'app-pokemon-details',
  templateUrl: 'pokemon-details.component.html',
  styleUrls: ['pokemon-details.component.css'],
  providers: [PokemonDetailsService],
  pipes: [KeyValues]
})
export class PokemonDetailsComponent implements OnInit {

  pokemonDetails = {};
  private _routeSegmentParam;
  constructor(private _segment:RouteSegment,
              private _pokemonDetailsService:PokemonDetailsService,
              public _sharedService: SharedService) {}

  ngOnInit() {
    this._routeSegmentParam = this._segment.parameters;
    this._pokemonDetailsService.getPokemonDetails(this._routeSegmentParam.id)
    .subscribe((pokemonDetails:any) => {
      this.pokemonDetails = pokemonDetails;
      this._sharedService.addPokemonImage(this.pokemonDetails, pokemonDetails.id-1);
      console.log(this.pokemonDetails);
    });
  }

}
