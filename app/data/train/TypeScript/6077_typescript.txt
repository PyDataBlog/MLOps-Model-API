import {Injectable} from "@angular/core";
import {Store} from "../app.store";
import {ENDSEKER} from '../constants/actions';

/// these EndSekerActions are not necessary, as it can be calculated from the state.list . 
/// leaving this for now for later purposes\reference only. 

@Injectable()
export class EndSekerActions {

  private store:Store;

  constructor(_store:Store) {
    this.store = _store;
  }

  setResult(item) {
    this.setProperty(ENDSEKER.END, { noResults: item });
  }

  setResultYear(item) {
    this.setProperty(ENDSEKER.CALC_YEAR, { resultYear: item });
  }

  setResultPercent(item) {
    this.setProperty(ENDSEKER.CALC_PERCENT, { resultPercent: item });
  }


  private setProperty(type, item) {
    this.store.dispatch({
      type   : type,
      payload: item
    })
  }

}