import { InMemoryDbService } from 'angular-in-memory-web-api';
export class InMemoryDataService implements InMemoryDbService {
  createDb() {
    let heroes = [
        { id: 10, name: 'Eduardo C', secretIdentity: 'Eduardo Allen'},
        { id: 11, name: 'Débora', secretIdentity: 'Eduardo Allen'},
        { id: 12, name: 'Daniel', secretIdentity: 'Eduardo Allen'},
        { id: 13, name: 'Max', secretIdentity: 'Eduardo Allen'},
        { id: 14, name: 'Super Edu', secretIdentity: 'Eduardo Allen'},
        { id: 15, name: 'The Flash', secretIdentity: 'Eduardo Allen'},
        { id: 16, name: 'Lucas', secretIdentity: 'Eduardo Allen'},
        { id: 17, name: 'Isabela', secretIdentity: 'Eduardo Allen'},
        { id: 18, name: 'Dr IQ', secretIdentity: 'Eduardo Allen'},
        { id: 19, name: 'Magma', secretIdentity: 'Eduardo Allen'},
        { id: 20, name: 'Tornado', secretIdentity: 'Eduardo Allen'}
    ];
    return {heroes};
  }
}