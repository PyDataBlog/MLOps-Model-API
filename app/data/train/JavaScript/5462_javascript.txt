/* 
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

//var winMae;
var wins;

//function mae2(idLink, maeNombre, winParent) {
//    //alert(idLink.text());
//    
//    //alert($(idLink).text());
//    
//    var wins = new dhtmlXWindows();
//    skin="dhx_terrace";
//    wins.setSkin(skin);
//    winId="win"+maeNombre;
//    wins.createWindow(winId, 1, 1, 900, 530);       
//    wins.window(winId).setText("<h4>" + $(idLink).text() + "</h4>");
//    wins.window(winId).setModal(true);
//    wins.window(winId).button("minmax").disable();    
//    wins.window(winId).centerOnScreen();    
//    wins.window(winId).progressOn();    
//    wins.window(winId).setIconCss("without_icon");
//    var urlParametros={'maeTipoId':maeTipoId, 'winParent': winParent, 'maeParametros': maeParametros};
//    var url = Routing.generate('maestro', urlParametros);
//    wins.window(winId).attachURL(url);
//        
//    wins.window(winId).attachEvent("onContentLoaded", function(id){
//        //alert(id.id);
//        id.progressOff();        
//        var ifr = id.getFrame();
//        ifr.contentWindow.wins=wins;
//        //ifr.contentWindow.winParent=winParent;
//        //alert(ifr.contentWindow.winParent);
//    });
//    
//     
////    alert(maeParametros);
////    var parametros = JSON.parse(maeParametros);
////    alert(parametros);
//////    
////    for(x=0; x<parametros.length; x++) {
////        alert(parametros[x].maeNombre);
////        //console.log(parametros[x].description);
////    }    
//}

function mae(maeTipoId, winParent, maeParametros) {
    var wins = new dhtmlXWindows();
    skin="dhx_terrace";
    wins.setSkin(skin);
    var url="";
    switch (maeTipoId) {
        case "1": //perfiles
            winId="winPerfil";
            //wins.createWindow(winId, 1, 1, 90, 60);
            wins.createWindow(winId, 1, 1, 900, 630);
            //wins.window(winId).setText("<h5>Perfil de cargo</h5>");                        
            wins.window(winId).setText("Perfil de cargo");                        
            var url = Routing.generate('perfil');            
            break;
        case "2": //personas
            winId="winPersona";
            wins.createWindow(winId, 1, 1, 900, 530);
            wins.window(winId).setText("Personas");
            var url = Routing.generate('persona');
            break;
        case "3": //competencias
            winId="winCompetencia";
            wins.createWindow(winId, 1, 1, 900, 600);
            wins.window(winId).setText("<h5>UCL's</h5>");
            var url = Routing.generate('competencia');            
            break;
        case "4": //actividadClave
            winId="winActvidadClave";
            wins.createWindow(winId, 1, 1, 700, 450);
            wins.window(winId).setText("<h5>Competencia - Actividades Claves</h5>");
            break;
        case "5": //aprendizajes                
            winId="winAprendizaje";
            wins.createWindow(winId, 1, 1, 900, 530);
            wins.window(winId).setText("<h5>Conocimiento - Aprendizajes</h5>");            
            
            break;
        case "6": //conocimientos
            winId="winConocimiento";
            wins.createWindow(winId, 1, 1, 900, 630);
            wins.window(winId).setText("<h5>Competencias Conductuales</h5>");                                    
            var url = Routing.generate('conocimiento');
            break;
        case "7": //pregunta
            winId="winPregunta";
            wins.createWindow(winId, 1, 1, 900, 630);
            wins.window(winId).setText("Cuestionario Evaluacion");
            parametros=[{'preguntaTipoId': maeParametros}];
            var maeParametros = JSON.stringify(parametros);
            break;
        case "8": //clientes
            winId="winCliente";
            wins.createWindow(winId, 1, 1, 900, 630);
            wins.window(winId).setText("Clientes");                        
            var url = Routing.generate('empresa');
            break;        
        case "9": //cursos
            winId="winCurso";
            wins.createWindow(winId, 1, 1, 900, 630);
            wins.window(winId).setText("Cursos");                       
            break;       
        case "11": //evaluaciones
            winId="winEvaluacion";
            wins.createWindow(winId, 1, 1, 900, 630);
            wins.window(winId).setText("Evaluaciones");    
            var url = Routing.generate('evaluacion');
            break;         
    }
//    switch (maeTipoId) {
//        case 1:
//        case 2:
//        case 3:
//        case 4:
//            winId="winEntidad";
//            wins.createWindow(winId, 1, 1, 900, 530);            
//            switch (maeTipoId) {        
//                case 1:
//                    wins.window(winId).setText("<h4>Clientes</h4>");
//                    break;
//                case 2:
//                    wins.window(winId).setText("<h4>Proveedores</h4>");
//                    break;                    
//                case 3:
//                    wins.window(winId).setText("<h4>Empresas de Factoring</h4>");
//                    break;                    
//                case 4:
//                    wins.window(winId).setText("<h4>Empresas Mandantes</h4>");
//                    break;                    
//            }
//            
//            break;
//        case 5:
//            winId="winEntidadDireccion";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Clientes - Direcciones</h4>");
//            break;
//        case 6:
//            winId="winGlosaPago";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Glosas de Pago</h4>");
//            break;            
//        case 7:
//            winId="winVendedor";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Vendedores</h4>");
//            break;
//        case 8:
//            winId="winComprador";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Compradores</h4>");
//            break;        
//        case 9:
//            winId="winObservacion";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Observaciones</h4>");
//            break;                    
//        case 10:
//            winId="winSucursal";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Sucursales</h4>");
//            break;                
//        case 11:
//            winId="winBodega";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Bodegas</h4>");
//            break;                    
//        case 12:
//            winId="winObra";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Obras</h4>");
//            break; 
//        case 13:
//            winId="winCuentaBanco";
//            wins.createWindow(winId, 1, 1, 800, 430);
//            wins.window(winId).setText("<h4>Cuentas Bancos</h4>");
//            break;
//        case 14:
//            winId="winProducto";
//            wins.createWindow(winId, 1, 1, 900, 530);       
//            wins.window(winId).setText("<h4>Servicios o Productos</h4>");
//            break;            
//        case 15:
//            winId="winProductoFamilia";
//            wins.createWindow(winId, 1, 1, 800, 430);       
//            wins.window(winId).setText("<h4>Productos o Servicios - Familias</h4>");
//            break;            
//        case 16:
//            winId="winListaPrecio";
//            wins.createWindow(winId, 1, 1, 800, 430);       
//            wins.window(winId).setText("<h4>Lista de Precios</h4>");
//            break;            
//        case 17:
//            winId="winCentroCosto";
//            wins.createWindow(winId, 1, 1, 800, 430);       
//            wins.window(winId).setText("<h4>Centros de Costo</h4>");
//            break;
//        case 18:
//            winId="winCurso";
//            wins.createWindow(winId, 1, 1, 900, 530);       
//            wins.window(winId).setText("<h4>Cursos</h4>");
//            break;        
//        case 19:
//            winId="winRelator";
//            wins.createWindow(winId, 1, 1, 900, 530);       
//            wins.window(winId).setText("<h4>Relatores</h4>");
//            break;
//    }
    
    //win.window("VtnResumenLibro").
    
    wins.window(winId).setModal(true);
    //winReferenciaForm.setText("<span class='text_title_header'>Header Text</span>");
    

    //winReferenciaForm.button("close").disable();
    wins.window(winId).button("minmax").disable();
    //winFac.attachObject("dvReferencias", false);
    wins.window(winId).centerOnScreen();
    //winReferenciaForm.attachHTMLString("hola!");
    //dhxWins.window(id).progressOn();
    wins.window(winId).progressOn();
    //dhxWins.window(id).setIconCss(css);
    //wins.window(winId).setIconCss("without_icon");
    
    
    
    //var urlParametros={'maeTipoId':maeTipoId, 'winParent': winParent, 'maeParametros': maeParametros};
    //var url = Routing.generate('maestro', urlParametros);
    //alert(url);
    wins.window(winId).attachURL(url);
        
    wins.window(winId).attachEvent("onContentLoaded", function(id){
        //alert(id.id);
        id.progressOff();        
        var ifr = id.getFrame();
        ifr.contentWindow.wins=wins;
        //ifr.contentWindow.winParent=winParent;
        //alert(ifr.contentWindow.winParent);
    });               
}

function maeVolver(maeTipoId, winParent, winName, maeParametros) {
    winId=winName;
    wins.window(winId).progressOn(); 
    var jsnParametros = JSON.stringify(maeParametros);
    var urlParametros = {'maeTipoId': maeTipoId, 'winParent':winParent, 'maeParametros': jsnParametros};
    var url=Routing.generate('maestro', urlParametros);            
    document.location.href=url;
}                                                        



        
        



