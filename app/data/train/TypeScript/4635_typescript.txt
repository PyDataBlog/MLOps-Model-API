import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-eventbinding',
  templateUrl: './eventbinding.component.html',
  styleUrls: ['./eventbinding.component.css']
})
export class EventbindingComponent implements OnInit {

  texto = "Texto de prueba";

  constructor() { }

  ngOnInit() {
  }

  modTexto(){
    this.texto = "ALV prro :v";
  }

  cambTexto(){
    alert("El texto anterior era: " + this.texto);
    this.texto = "Nuevo texto de prueba";
    alert("El nuevo texto será: " + this.texto);
  }

}
