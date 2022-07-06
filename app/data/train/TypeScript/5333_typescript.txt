import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { FormularioComponent } from './formulario/formulario.component';
import { ListaComponent } from './lista/lista.component';
import { NoticiaComponent } from './noticia/noticia.component';
import { FormdinComponent } from './formdin/formdin.component';

@NgModule({
  imports: [
    CommonModule,
    FormsModule
  ],
  exports: [
    FormularioComponent,
    ListaComponent,
    NoticiaComponent,
    FormdinComponent
  ],
  declarations: [
    FormularioComponent,
    ListaComponent,
    NoticiaComponent,
    FormdinComponent
  ]
})
export class LogicaModule { }
