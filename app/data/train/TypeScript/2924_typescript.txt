import {Component} from 'angular2/core';

export class Reward{
    id: number;
    name: string;
    description: string;
    points: number;
}

@Component({
    selector: 'my-app',
    templateUrl: 'templates/reward-detail.html'
})

export class AppComponent {
    title: string;
    title = 'Reward';

    reward: Reward = {
        id: 1,
        name: 'Una noche en el hotel XX',
        description: 'Hotel de lujo 5 estrellas con acomodación doble',
        points: '100'
    };
}