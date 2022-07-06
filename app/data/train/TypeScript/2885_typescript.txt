import {Injectable} from '@angular/core';
import {Headers, Http, Response} from '@angular/http';
import 'rxjs/add/operator/toPromise';
import {Book} from './../models/book';

@Injectable()
export class BookService {
    private booksUrl: string = 'api/books';

    constructor(private http: Http) {}

    getBooks(): Promise<Book[]> {
        return this.http.get(this.booksUrl)
            .toPromise()
            .then(response => response.json())
            .catch(this.handleError);
    }

    getBook(id: string) {
        return this.http.get(`${this.booksUrl}/${id}`)
            .toPromise()
            .then(response => response.json())
            .catch(this.handleError);
    }

    private handleError(error: any) {
        console.error('An error occurred', error);
        return Promise.reject(error.message || error);
    }
}

