import { TestBed, inject } from '@angular/core/testing';

import { EditorPanelService } from './editor-panel.service';

describe('EditorPanelService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [EditorPanelService]
    });
  });

  it('should be created', inject([EditorPanelService], (service: EditorPanelService) => {
    expect(service).toBeTruthy();
  }));
});
