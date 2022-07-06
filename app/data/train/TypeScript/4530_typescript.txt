import { Component } from '@angular/core';

@Component({
    selector: 'main-footer',
    template: `
        <div id="main-footer" class="main-footer">
            <div class="container">
                <div class="row">
                    <div class="col-lg-12">
                        <div class="navbar-menu">
                            <span class="smiley">☺</span> Made by MATR Participants
                        </div>
                    </div>
                </div>
            </div>
        </div>
    `,
    styles: [`
        h1 {
            margin: 0;
        }
        div.main-footer {
            background-color:#0066B3;
            color: white;
            padding: 10px;
            border: none;
            height:50px;
            border-radius: 0px;
            position: absolute;
            bottom: 0;
            width: 100%;
            /* Set the fixed height of the footer here */
            line-height: 50px;
            height: 70px;
        }
        div.navbar-menu {
            text-align:center;
        }
        ul.navbar-menu {
            list-style-type: none;
            margin: 0;
            padding: 0;
            overflow: hidden;
        }
        li.navbar-menu {
            display: block;
            text-align: center;
            text-decoration: none;
            color:black;
        }
        .btn-main-custom {
            text-align: center;
            cursor: pointer;
            line-height: 60px;
            text-decoration: none;
            padding: 0px 5px 0px 5px;
            border: 0px solid #0066B3;
            display: block;
            color: #0066B3;
            font-size: 14px;
            font-weight: 400;
            border-radius: 2px;
            margin-right:10px;
            min-width:50px;
        }
        .btn-main-custom:after {
            content: '';
            width: 0;
            position: absolute;
            -webkit-transition: .2s;
            transition: .2s;
            background: #0066B3;
            left: 50%;
            bottom: 0;
            height: 5px;
        }
        .btn-main-custom:hover:after {
            width: 100%;
            left: 0;
        }
        a.navbar-logo {
            text-decoration: none;
        }
        a.navbar-menu:hover, a.navbar-menu:visited {
            background-color:transparent;
        }
        a.active {
            color:black !important;
        }
        span.smiley {
            font-size: 25px;
        }
    `]
})
export class MainFooterComponent {
}