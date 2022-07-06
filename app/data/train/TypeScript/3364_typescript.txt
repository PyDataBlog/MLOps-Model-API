import { Component, OnInit, HostListener } from '@angular/core';

@Component({
  selector: 'app-landing-page',
  templateUrl: './landing-page.component.html',
  styleUrls: ['./landing-page.component.scss']
})
export class LandingPageComponent implements OnInit {
  constructor() { }

  @HostListener('window:scroll', ['$event'])
  onScroll(event) {
    const wScroll = window.scrollY;

    if (wScroll <= document.getElementById('parallax-header').clientHeight) {
      const parallaxLayer1 = <HTMLElement>document.getElementById('parallax-layer-1');
      parallaxLayer1.style.transform = 'translate(0px, ' + wScroll / 5 + '%)';
      const parallaxLayer2 = <HTMLElement>document.getElementById('parallax-layer-2');
      parallaxLayer2.style.transform = 'translate(0px, ' + wScroll / 4 + '%)';
      const parallaxLayer3 = <HTMLElement>document.getElementById('parallax-layer-3');
      parallaxLayer3.style.transform = 'translate(0px, ' + wScroll / 8 + '%)';
    }
  }

  ngOnInit() {
  }
}
