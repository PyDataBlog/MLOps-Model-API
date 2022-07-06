import { NgModule, ModuleWithProviders } from '@angular/core';
import { CommonModule } from "@angular/common";

import { SharedComponent } from './src/components/shared.component';
import { SharedService } from './src/services/shared.service';

export * from './src/components/shared.component';
export * from './src/services/shared.service';

@NgModule({
  imports: [CommonModule],
  declarations: [SharedComponent],
  exports: [SharedComponent],
  providers: [],
})
export class SharedModule {
    static forRoot(): ModuleWithProviders {
        return {
            ngModule: SharedModule,
            providers: [SharedService]
        }
    }
 }