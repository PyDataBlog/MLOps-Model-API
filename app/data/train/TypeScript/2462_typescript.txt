/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { HotKeywordApiService } from './hot-keyword-api.service';

describe('HotKeywordApiService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [HotKeywordApiService]
    });
  });

  it('should ...', inject([HotKeywordApiService], (service: HotKeywordApiService) => {
    expect(service).toBeTruthy();
  }));
});
