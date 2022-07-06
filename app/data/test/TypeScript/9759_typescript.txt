import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { RouterModule }   from '@angular/router';
import { FormsModule }   from '@angular/forms'; // <-- NgModel lives here
import { HeroDetailComponent } from './hero-detail.component';

import { AppComponent } from './app.component';
import { HeroesComponent }     from './heroes.component';
import { HeroService }         from './hero.service';
import { AppRoutingModule }     from './app-routing.module';
import { DashboardComponent }   from './dashboard.component';

@NgModule({
  declarations: [
    AppComponent,
    HeroDetailComponent,
    HeroesComponent,
    DashboardComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    AppRoutingModule
  ],
  providers: [HeroService],
  bootstrap: [AppComponent]
})
export class AppModule { }
