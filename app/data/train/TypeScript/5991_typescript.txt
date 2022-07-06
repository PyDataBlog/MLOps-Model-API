import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';

import { AppComponent } from './app.component';
import { SearchComponent } from './components/search/search.component';
import { NavComponent } from './components/nav/nav.component';
import { SelectedPlacesComponent } from './components/selected-places/selected-places.component';
import { HomeComponent } from './components/home/home.component';
import { SettingComponent } from './components/setting/setting.component';

import { GooglePlacesService } from './services/google-places/google-places.service';
import { SelectedPlacesService } from './services/selected-places/selected-places.service';
import { GeolocationService } from './services/geolocation/geolocation.service';

const routes: Routes = [
  { path: '', component: HomeComponent },
  { path: 'selected-places', component: SelectedPlacesComponent },
  { path: 'setting', component: SettingComponent }
];

@NgModule({
  declarations: [
    AppComponent,
    SearchComponent,
    NavComponent,
    SelectedPlacesComponent,
    HomeComponent,
    SettingComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    RouterModule.forRoot(routes)
  ],
  providers: [
    GooglePlacesService,
    SelectedPlacesService,
    GeolocationService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
