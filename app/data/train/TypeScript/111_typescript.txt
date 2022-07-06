import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

import { ExtendComponent } from './src/components/extend.component';
import { SubExtendComponent } from './src/components/sub-extend.component';

const extendRoutes: Routes = [
  {
    path: '',
    component: ExtendComponent,
  },
  {
    path: 'main',
    component: ExtendComponent,
  },
  {
    path: 'sub',
    component: SubExtendComponent
  },
];
@NgModule({
  imports: [
    RouterModule.forChild(extendRoutes)
  ],
  exports: [
    RouterModule
  ]
})
export class ExtendRoutingModule {}