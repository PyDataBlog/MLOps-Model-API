import { Component, OnInit, ViewEncapsulation , ElementRef } from '@angular/core';
import { NgForm } from '@angular/forms';
import { ActivatedRoute, Params, Router } from "@angular/router";
import { DiasRepartoService } from './diasReparto.service';
import { ListadoCooperativaService } from '../listadoCooperativa.service';

@Component({
  selector: 'app-DiasReparto',
  templateUrl: "./diasReparto.component.html",
  providers: [
    DiasRepartoService,
    ListadoCooperativaService
  ]
})
export class DiasRepartoComponent implements OnInit {
  title = 'Días reparto';
  public searchElementRef: ElementRef;
  exitoso: any = false;
  diaReparto: any = {
    'cooperativa': -1
  };
  diasReparto: any;
  cooperativas: any[];
  cooperativa_seleccionada: any;


  constructor(private diasRepartoService: DiasRepartoService,
              private cooperativaService: ListadoCooperativaService,
              private route: ActivatedRoute,
              private router: Router) {
  }

   ngOnInit() {
      this.cooperativaService.getCooperativas()
        .subscribe(response => {
          this.cooperativas = response;
        });

        this.route.params.subscribe(params => {
           this.diaReparto.cooperativa = +params['id'];
        });

        this.route.params
          .switchMap((params: Params) =>
            this.diasRepartoService.getDiasReparto(+params['id'])
          ).subscribe(diasreparto => {
          for (const diareparto of diasreparto) {
            if (diareparto['dia'] == '1.0') {
              this.diaReparto.lunes = diareparto['activo'];
            }
            if (diareparto['dia'] == '2.0') {
              this.diaReparto.martes = diareparto['activo'];
            }
            if (diareparto['dia'] == '3.0') {
              this.diaReparto.miercoles = diareparto['activo'];
            }
            if (diareparto['dia'] == '4.0') {
              this.diaReparto.jueves = diareparto['activo'];
            }
            if (diareparto['dia'] == '5.0') {
              this.diaReparto.viernes = diareparto['activo'];
            }
            if (diareparto['dia'] == '6.0') {
              this.diaReparto.sabado = diareparto['activo'];
            }
            if (diareparto['dia'] == '7.0') {
              this.diaReparto.domingo = diareparto['activo'];
            }
          }
        });
   }

  guardarDiaReparto(formDiaReparto: NgForm) {
      this.exitoso = false;
      const cooperativa_id = this.diaReparto.cooperativa;
      if (cooperativa_id > 0) {
          this.diasRepartoService.guardarDiaReparto(this.diaReparto).subscribe(response => {
            this.router.navigateByUrl('cooperativa/diasreparto/consultar/' + this.diaReparto.cooperativa);
            this.exitoso = true;
          });
      } else {
        alert( 'Debe seleccionar una cooperativa.' );
      }
  }

}
