import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule }   from '@angular/forms';
import { HttpModule, JsonpModule } from '@angular/http';

import { routing }        from './app.routing';

import { AppComponent }  from './components/app.component';
import { LoginModalComponent } from "./components/login-modal.component";
import { HomeComponent } from "./components/home.component";
import { MainNavComponent } from "./components/main-nav.component";
import { SecondPotComponent } from "./components/second-pot.component";
import { AuthService } from "./services/auth.service";
import { SettingsService } from "./services/settings.service";
import { PlayersService } from "./services/players.service";

import { Ng2Bs3ModalModule } from 'ng2-bs3-modal/ng2-bs3-modal';
import {CommonModule} from "@angular/common";

@NgModule({
  imports:      [
    CommonModule,
    BrowserModule,
    FormsModule,
    routing,
    Ng2Bs3ModalModule,
    HttpModule,
    JsonpModule
  ],
  declarations: [
    AppComponent,
    LoginModalComponent,
    HomeComponent,
    MainNavComponent,
    SecondPotComponent
  ],
  providers: [
    AuthService,
    SettingsService,
    PlayersService
  ],
  bootstrap:    [ AppComponent ]
})
export class AppModule { }
