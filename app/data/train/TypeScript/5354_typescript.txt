import { Injectable } from '@angular/core';
import { Resolve, ActivatedRouteSnapshot } from '@angular/router';
import { MessageService } from '../core/messages.service';

@Injectable()
export class UserMessagesResolve implements Resolve<any> {

    constructor(private messageService: MessageService) {}

    resolve(route: ActivatedRouteSnapshot) {

    return "user";
}
}