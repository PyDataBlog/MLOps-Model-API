import { Component, OnInit } from '@angular/core';
import { GalleryService } from 'ng-gallery';
import { fadeInAnimation } from '../../animation-fade-in';

@Component({
  moduleId: module.id,
  selector: 'sd-enquire-client-portal',
  templateUrl: 'enquire-client-portal.component.html',
  animations: [fadeInAnimation],
  host: { '[@fadeInAnimation]': '', 'class': 'animate-router' }
})
export class EnquireClientPortalComponent implements OnInit {

  images = [
    {
      src: 'images/portfolio/enquire-client-portal/register.gif',
    },
    {
      src: 'images/portfolio/enquire-client-portal/cp-1.png',
    },
    {
      src: 'images/portfolio/enquire-client-portal/cp-2.png',
    },
    {
      src: 'images/portfolio/enquire-client-portal/cp-3.png',
    },
    {
      src: 'images/portfolio/enquire-client-portal/cp-4.png',
    },
  ];

  constructor(private gallery: GalleryService) {
  }

  ngOnInit() {
    this.gallery.load(this.images);
  }

}
