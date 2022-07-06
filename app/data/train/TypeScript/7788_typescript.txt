import { MaterializeModule } from "angular2-materialize";

import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { ModuleWithProviders } from '@angular/core';

import { AppComponent } from './app.component';
import { AppRoutingModule } from './app-routing.module';
import { HeaderComponent } from './header/header.component';
import { FooterComponent } from './footer/footer.component';
import { VideoIntroComponent } from './video-intro/video-intro.component';
import { InfoComponent } from './info/info.component';
import { MapComponent } from './map/map.component';
import { OrganizesComponent } from './organizes/organizes.component';
import { AuspicesComponent } from './auspices/auspices.component';
import { ContactComponent } from './contact/contact.component';

import { AgmCoreModule } from 'angular2-google-maps/core';
import { SpotComponent } from './spot/spot.component';
import { AreasComponent } from './areas/areas.component';
import { InscriptionComponent } from './inscription/inscription.component';
import { MainComponent } from './main/main.component';


@NgModule({
  declarations: [
    AppComponent,
    HeaderComponent,
    FooterComponent,
    VideoIntroComponent,
    InfoComponent,
    MapComponent,
    OrganizesComponent,
    AuspicesComponent,
    ContactComponent,
    SpotComponent,
    AreasComponent,
    InscriptionComponent,
    MainComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule,
    AppRoutingModule,

    AgmCoreModule.forRoot({
      apiKey: 'AIzaSyCfymJOdkazl6fqIcBnGAdmU9aDxtHIOT8'
    }),

    MaterializeModule

  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
