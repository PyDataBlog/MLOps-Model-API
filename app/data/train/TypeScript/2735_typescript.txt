import { Routes, RouterModule }  from '@angular/router';
import { interestsComponent } from './interests.component';

const routes: Routes = [
  {
    path: '',
    component: interestsComponent
  }
];

 export const routing = RouterModule.forChild(routes);
 