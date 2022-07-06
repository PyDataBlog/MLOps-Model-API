/// <reference path="../typings/tsd.d.ts" />

import { QueryActionCreator } from "./actions/query-actions";
import { AppComponent } from "./components/app";
import { setQueryReducer } from "./reducers/query-reducers";


var app:any = angular.module("app", [
    "localStorageManager",
    "safeDigest",
    "store",
]);

app.config(["reducersProvider", reducersProvider => {
    reducersProvider.configure(setQueryReducer);
}]);

app.service("queryActionCreator", ["dispatcher", QueryActionCreator]);

app.component((<any>AppComponent).config);

app.config(["initialStateProvider", "localStorageManagerProvider", (initialStateProvider, localStorageManagerProvider) => {
    if (!localStorageManagerProvider.get({ name: "initialState" }))
        localStorageManagerProvider.put({
            name: "initialState", value: { query: null }
        });
    initialStateProvider.configure(localStorageManagerProvider.get({ name: "initialState" }));
}]);