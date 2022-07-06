import { TestBed, inject } from '@angular/core/testing';
import { SamplesService } from './samples.service';

describe('SamplesService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [SamplesService]
    });
  });

  it('should ...', inject([SamplesService], (service: SamplesService) => {
    expect(service).toBeTruthy();
  }));
});
