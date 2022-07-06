/* 
 * Copyright (C) 2014 rafa
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */


function fNotaRoutes() {

//    Path.map("#/nota").to(function () {
//        $('#indexContenidoJsp').spinner();
//        control('nota').list($('#indexContenido'), param().defaultizeUrlObjectParameters({}), null);
//        //notaControl.modalListEventsLoading(notaObject, notaView, $('#indexContenido'), param().defaultizeUrlObjectParameters({}), null);        
//        $('#indexContenidoJsp').empty();
//        return false;
//    });

    Path.map("#/nota").to(function () {
        $('#indexContenidoJsp').spinner();
        oNotaControl.list($('#indexContenido'), param().defaultizeUrlObjectParameters({}), null, oNotaModel, oNotaView);
        //notaControl.modalListEventsLoading(notaObject, notaView, $('#indexContenido'), param().defaultizeUrlObjectParameters({}), null);        
        $('#indexContenidoJsp').empty();
        //$('#indexContenidoJsp').append(oNotaControl.getClassNameNota());
        return false;
    });

    Path.map("#/nota/list/:url").to(function () {
        $('#indexContenidoJsp').spinner();
        var paramsObject = param().defaultizeUrlObjectParameters(param().getUrlObjectFromUrlString(this.params['url']));
        oNotaControl.list($('#indexContenido'), paramsObject, null, oNotaModel, oNotaView);
        $('#indexContenidoJsp').empty();
        return false;
    });

    Path.map("#/nota/view/:id").to(function () {
        $('#indexContenidoJsp').spinner();
        var paramsObject = param().defaultizeUrlObjectParameters(param().getUrlObjectFromUrlString(this.params['url']));
        oNotaControl.view($('#indexContenido'), paramsObject['id'], oNotaModel, oNotaView);
        $('#indexContenidoJsp').empty();

        return false;
    });

    Path.map("#/nota/edit/:id").to(function () {
        $('#indexContenidoJsp').spinner();
        var paramsObject = param().defaultizeUrlObjectParameters(param().getUrlObjectFromUrlString(this.params['url']));
        oNotaControl.edit($('#indexContenido'), paramsObject['id'], oNotaModel, oNotaView);
        $('#indexContenidoJsp').empty();
    });
    Path.map("#/nota/new").to(function () {
        $('#indexContenidoJsp').spinner();
        //var paramsObject = param().defaultizeUrlObjectParameters(param().getUrlObjectFromUrlString(this.params['url']));
        oNotaControl.new($('#indexContenido'), null, oNotaModel, oNotaView);
        $('#indexContenidoJsp').empty();
        return false;
    });
    Path.map("#/nota/new/:url").to(function () {
        $('#indexContenidoJsp').spinner();
        var paramsObject = param().defaultizeUrlObjectParameters(param().getUrlObjectFromUrlString(this.params['url']));
        oNotaControl.new($('#indexContenido'), paramsObject, oNotaModel, oNotaView);
        $('#indexContenidoJsp').empty();
        return false;
    });

    Path.map("#/nota/remove/:id").to(function () {
        $('#indexContenidoJsp').spinner();
        var paramsObject = param().defaultizeUrlObjectParameters(param().getUrlObjectFromUrlString(this.params['url']));
        oNotaControl.remove($('#indexContenido'), paramsObject['id'], oNotaModel, oNotaView);
        $('#indexContenidoJsp').empty();
        return false;
    });
}