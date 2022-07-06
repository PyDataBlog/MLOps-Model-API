import {BrowserModule} from '@angular/platform-browser';
import {NgModule} from '@angular/core';

import {FormsModule} from '@angular/forms';

import {AppComponent} from './app.component';
import {AppTableComponent} from './app-table.component';

@NgModule({
    declarations: [
        AppComponent,
        AppTableComponent
    ],
    imports: [
        BrowserModule,
        FormsModule
    ],
    providers: [],
    bootstrap: [AppComponent]
})
export class AppModule {
}
