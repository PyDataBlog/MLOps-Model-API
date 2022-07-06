import 'reflect-metadata';

// import {enableProdMode} from 'angular2/core';
import {bootstrap} from 'angular2/platform/browser';
import {ROUTER_PROVIDERS} from 'angular2/router';

import {App} from './app/app'
// font faces and icons available site wide
// .styl files will use the font-faces
// .html file will use the icons
// webpack will url-inline them into bundle.js
import './assets/fonts/font_faces.styl';

bootstrap(
  App,
  [
    ROUTER_PROVIDERS,
  ]
);
