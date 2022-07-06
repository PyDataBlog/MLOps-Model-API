 var baseURL 	=  window.location.pathname;
 var webRoot 	= "/"+baseURL.split("/")[1];

$(function(){


  $('#logo_top').click(function() {

			window.location = webRoot;

	});
  $('.link_formate').click(function() {
		$('.selected_menu').removeClass('selected_menu');
		$(this).addClass('selected_menu');
		window.location = webRoot+"/Formate";
	});
	$('.link_iniciativas').click(function() {
		$('.selected_menu').removeClass('selected_menu');
		$(this).addClass('selected_menu');
		window.location = webRoot+"/Iniciativas";
	});
	$('.link_perfil').click(function() {
		$('.selected_menu').removeClass('selected_menu');
		$(this).addClass('selected_menu');
		window.location = webRoot+"/usuarios/perfil";
	});
	$('.btn_usuario').click(function() {
			let id  = $(this).attr('id').split('_')[2];
			window.location = webRoot+"/usuarios/perfil/"+id;
	});
	$('#btn_abrirSideMenu').click(function() {
			$('#sideMenu').show();
			$(this).hide();
			$('#btn_cerrarSideMenu').show();
	});
	$('#btn_cerrarSideMenu').click(function() {
			$('#sideMenu').hide();
			$(this).hide();
			$('#btn_abrirSideMenu').show();
	});


});

function mainMenu() {
	console.log('click');
   if($('#mainMenu').is(':visible')){
   		$('#mainMenu').hide();
   }
   else
   		$('#mainMenu').show();
}


//Funcion CSS
function ajustarMarginTop(elementoId,newMarginTop){
	var elemento = document.getElementById(elementoId);
	var cssMarginTop = $('#'+elementoId).css("margin-top");
	//var cssMargin = $('#'+elementoId).css("margin").split(" ");
	//elemento.style.margin = newMarginTop+"px "+cssMargin[1]+" "+cssMargin[2]+" "+cssMargin[3] ;
	//elemento.style.margin = $('#'+elementoId).css("margin").replace(cssMarginTop,newMarginTop+"px");;
}
