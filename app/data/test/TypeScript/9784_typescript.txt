import { Component } from '@angular/core';

@Component({
    selector: 'css-list-groups',
    template: `
        <ng2vd-content-wrapper component="List Groups">
            <ng2vd-example-box demoTitle="Basic Example"
                [snippets]="snippets" component="Basic Example" demo="basicExample">
                <div [htmlTemplate]="'./list-groups/basic-example.html'"></div>
            </ng2vd-example-box>
            
            <ng2vd-example-box demoTitle="List Badges"
                [snippets]="snippets" component="List Badges" demo="listBadges">
                <div [htmlTemplate]="'./list-groups/list-badges.html'"></div>
            </ng2vd-example-box>

            <ng2vd-example-box demoTitle="Button Items"
                [snippets]="snippets" component="Button Items" demo="buttonItems">
                <div [htmlTemplate]="'./list-groups/button-items.html'"></div>
            </ng2vd-example-box>

            <ng2vd-example-box demoTitle="Linked Items"
                [snippets]="snippets" component="Linked Items" demo="linkedItems">
                <div [htmlTemplate]="'./list-groups/linked-items.html'"></div>
            </ng2vd-example-box>

            <ng2vd-example-box demoTitle="Custom Content"
                [snippets]="snippets" component="Custom Content" demo="customContent">
                <div [htmlTemplate]="'./list-groups/custom-content.html'"></div>
            </ng2vd-example-box>

        </ng2vd-content-wrapper>
    `,
})
export class ListGroupsComponent {

    snippets = {
        basicExample: {
            markup: require('!!prismjs-loader?lang=markup!./basic-example.html')
        },
        listBadges: {
            markup: require('!!prismjs-loader?lang=markup!./list-badges.html')
        },
        buttonItems: {
            markup: require('!!prismjs-loader?lang=markup!./button-items.html')
        },
        linkedItems: {
            markup: require('!!prismjs-loader?lang=markup!./linked-items.html')
        },
        customContent: {
            markup: require('!!prismjs-loader?lang=markup!./custom-content.html')
        }
    };
}