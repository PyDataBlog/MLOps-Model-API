import { NgModule } from '@angular/core';

import { SharedModule } from '../shared/shared.module';
import { ArchivesRoutingModule } from './archives-routing.module';

import { ArchiveDetailResolve } from './archive-detail/archive-detail-resolve.service';
import { ArchiveDetailComponent } from './archive-detail/archive-detail.component';
import { ArchivesComponent } from './archives.component';
import { ArchiveService } from './shared/archive.service';
import { ArchiveComponent } from './archive/archive.component';
import { ArchiveListComponent } from './archive-list/archive-list.component';

@NgModule({
  imports: [
    SharedModule,
    ArchivesRoutingModule
  ],
  declarations: [
    ArchivesComponent,
    ArchiveComponent,
    ArchiveDetailComponent,
    ArchiveComponent,
    ArchiveListComponent
  ],
  providers: [
    ArchiveService,
    ArchiveDetailResolve
  ]
})
export class ArchivesModule { }
