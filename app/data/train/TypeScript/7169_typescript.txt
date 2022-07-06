import { Routes, RouterModule }  from '@angular/router';
import { Pages } from './pages.component';

import { PageConstant } from './shared/page.consts';

// noinspection TypeScriptValidateTypes
const routes: Routes = [
  {
    path: PageConstant.PAGES_SELECTOR,
    component: Pages,
    children: [
      { path: '', redirectTo: 'home', pathMatch: 'full' },
      { path: 'home', loadChildren: () => require('./home/home.module') }
    ]
  }
];

export const routing = RouterModule.forChild(routes);
