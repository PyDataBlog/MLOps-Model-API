import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { DemoFormCadastroComponent } from './demo-form-cadastro.component';

describe('DemoFormCadastroComponent', () => {
  let component: DemoFormCadastroComponent;
  let fixture: ComponentFixture<DemoFormCadastroComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ DemoFormCadastroComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DemoFormCadastroComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
