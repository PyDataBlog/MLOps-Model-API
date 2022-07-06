import {Routes, RouterModule} from '@angular/router';

import {LoginPageComponent} from './components/login-page/login-page.component';
import {RegisterPageComponent} from './components/register-page/register-page.component';

const accountRoutes:Routes =  [{
  path: 'login',
  component: LoginPageComponent
}, {
  path: 'register',
  component: RegisterPageComponent
}];

export const accountRouting = RouterModule.forChild(accountRoutes);

