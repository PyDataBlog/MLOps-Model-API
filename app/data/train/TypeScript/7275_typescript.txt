//our root app component
import {NgModule} from '@angular/core'
import {BrowserModule} from '@angular/platform-browser'
import {App} from './app.component'
import {ViewportModule} from "./viewport/viewport.module"
import {OtherModule} from "./other/other.module"
import {MyRoutingModule} from "./app-routing.module"
import {HeaderComponent} from "./header.component"
import {NotFoundComponent} from "./not-found.component"
import {ContentComponent} from "./content/content.component";
import {LocalCommonModule} from "./common/local-common.module";
import {InvalidComponent} from "./invalid.component";
import {CanActivateIfChance} from "./common/simple.guard";
@NgModule({
  imports: [
    BrowserModule,
    LocalCommonModule,
    MyRoutingModule,
    ViewportModule,
    OtherModule
  ],
  declarations: [
    App,
    HeaderComponent,
    NotFoundComponent,
    ContentComponent,
    InvalidComponent
  ],
  providers: [
    CanActivateIfChance
  ],
  bootstrap: [ App ]
})
export class AppModule {}
