import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
//import {FormsModule} from '@angular/forms';
import { AppComponent } from './app.component';
import{MainuiComponent} from './components/mainui/mainui.component';
//import { UserComponent } from './components/user/user.component';
//import {DataService} from './services/data.service';
import { HttpModule } from '@angular/http';
import{RainService} from './services/rainservice.service';
import{GpsService} from './services/gpsservice.service';
//import{main}

@NgModule({
  declarations: [
    AppComponent,
    MainuiComponent
  //UserComponent
  ],
  imports: [
    BrowserModule,
    //FormsModule,
    HttpModule
  ],
  providers: [RainService,GpsService],
  bootstrap: [AppComponent],
})
export class AppModule { }
