import { Injectable } from "@angular/core";
import { Http } from "@angular/http";
import "rxjs/add/operator/map";
import {Observable} from "rxjs/Observable";

@Injectable()
export class ListadoCooperativaService {
    constructor(private http: Http) {
    }

    getCooperativas(): Observable<string[]> {
        return this.http.get("/cooperativa/cooperativalist")
            .map(response => <string[]>response.json());

    }
}
