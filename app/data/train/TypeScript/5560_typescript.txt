import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { toPromise } from 'rxjs/operator/toPromise';

import { NgbDateParserFormatter, NgbDateStruct } from '@ng-bootstrap/ng-bootstrap';

import { BaseService } from './base.service';
import { HolidayShutdown } from '../shared/dto/holiday-shutdown';
import { HolidayShutdownsUri } from '../shared/enums';

@Injectable()
export class HolidayShutdownsService extends BaseService {

  constructor(protected http: HttpClient) { super(http); }


  getHolidayShutdowns(): Promise<HolidayShutdown[]> {
    const uri = HolidayShutdownsUri.HolidayShutdownsBase;
    return this.http.get(`${uri}`)
      .toPromise()
      .then((response) => {
        return response as HolidayShutdown[];
      })
      .catch(this.handleError);
  }

  saveHolidayShutdown(holidayShutdown: HolidayShutdown): Promise<HolidayShutdown> {
    const uri = HolidayShutdownsUri.HolidayShutdownsBase;
    return this.http.post(`${uri}`, this.formatDate(holidayShutdown)).toPromise().catch(this.handleError);
  }

  deleteHolidayShutdown(id: number): Promise<HolidayShutdown> {
    const uri = HolidayShutdownsUri.HolidayShutdownsBase;
    return this.http.delete(`${uri}${id}`).toPromise().catch(this.handleError);
  }

  private formatDate(holidayShutdown: any): any {
    const from = holidayShutdown.from as NgbDateStruct;
    const to = holidayShutdown.to as NgbDateStruct;
    const copy = {
      id: holidayShutdown.id,
      'from': `${from.year}-${from.month}-${from.day}`,
      'to': `${to.year}-${to.month}-${to.day}`
    };
    return copy;
  }
}
