import { RouterModule, Routes } from '@angular/router';

import { BioComponent } from './components/bio.component';
import { BodyComponent } from './components/body.component';
import { QuoteComponent } from './components/quote.component';
import { VidComponent } from './components/vid.component';

const ROUTES: Routes = [
    { path: '', component: BodyComponent },
    { path: 'Biografía', component: BioComponent },
    { path: 'Documental', component: VidComponent },
    { path: 'Quotes', component: QuoteComponent }
];

export const app_routing = RouterModule.forRoot(ROUTES);