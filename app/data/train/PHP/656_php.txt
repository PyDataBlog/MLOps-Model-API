<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <title> </title>

<style>
.municipios{
      font-size:12px;
      font-family: 'helvetica','raleway';
      padding:1em 1em;
      background-color:#2C85AF;
      color:#FFFFFF;
}
.direccion{
      font-size:12px;
      font-family: 'helvetica','raleway';
      padding:1em 4em;
      background-color:#2C85AF;
      color:#FFFFFF;
}
.rellenotabla{
      font-size: 10px;
      padding:1em 1em;
      font-family: 'helvetica','raleway';
}
.rellenotabla2{
      font-size: 10px;
      padding:1em 3em;
      font-family: 'helvetica','raleway';
}
.textocolspan{
      font-size:10px;
      font-family: 'helvetica','raleway';
      background-color:#5AAED0;
      color:#FFFFFF;
      text-align:left;
      padding: 0.5em 1em;
      text-shadow: 1px 1px #000000;
}
@font-face {
font-family: 'helvetica';
src: url('fonts/helvetica-b.eot'); /* IE9 Compat Modes */
src: url('fonts/helvetica-b.eot?#iefix') format('embedded-opentype'), /* IE6-IE8 */
url('fonts/helveticaneue-webfont.woff') format('woff'), /* Modern Browsers */
url('fonts/HELVE6.ttf')  format('truetype'), /* Safari, Android, iOS */
url('fonts/Helvetica_Neue_typeface_weights.svg#svgFontName') format('svg'); /* Legacy iOS */
}
div.boton_ubicacion{
background-color: #035A9F;
color: #FFFFFF;
}

div.boton_ubicacion:hover{
background-color: #E4E5E6;
color: #035A9F;
}
div#terminales{
display:inline-block;
background-color:#2C85AF;
color:#FFFFFF;
font-family: 'raleway';
font-size:14px;padding:5px 15px;
cursor:pointer; 
cursor: hand;
margin-right:-4px;
}
div#terminales:hover{
background-color: #E4E5E6;
color: #035A9F;   
}
div#centros{
display:inline-block;
background-color:#2C85AF;
color:#FFFFFF;
font-family: 'raleway';
font-size:14px;
padding:5px 15px;
cursor:pointer; 
cursor: hand;
margin-right:-4px;
}
div#centros:hover{
background-color: #E4E5E6;
color: #035A9F;   
}
div#agencias{
display:inline-block;
background-color:#2C85AF;
color:#FFFFFF;
font-family: 'raleway';
font-size:14px;
padding:5px 15px;
cursor:pointer; 
cursor: hand;
margin-right:-4px;
}
div#agencias:hover{
background-color: #E4E5E6;
color: #035A9F;   
}
div#oxxo{
display:inline-block;
background-color:#2C85AF;
color:#FFFFFF;
font-family: 'raleway';
font-size:14px;
padding:5px 15px;
cursor:pointer; 
cursor: hand;
margin-right:-4px;
}
div#oxxo:hover{
background-color: #E4E5E6;
color: #035A9F;   
}

</style>

</head>
<body>
<div style="width:100%;border:1px solid #BDBDBD;text-align:center;overflow:hidden;float:right">


 <div id="terminales" >Terminales y Centrales</div> 

 <div id="centros" >Centros Comerciales</div> 

 <div id="agencias" >Agencias de Viajes</div> 

 <div id="oxxo" >Oxxo</div>



<!-- ============================TERMINALES Y CENTRALES================================================================= -->

<div id="mostrartermi" style="display:block">

      <div style="display:inline-block;text-align:left;font-family: 'helvetica','raleway'">
               <div style="display:inline-block"><img src="img/terminales.jpg" alt=""> </div>
               <div style="display:inline-block"><p style="font-size:12px">TERMINALES Y CENTRALES</p>
                <p style="color:#1959A0;font-weight:bold">SINALOA</p></div>
            </div>
      <div style="display:inline-block;width:100%;font-family: 'helvetica','raleway';">
            
            <div style="display:inline-block;padding:10px;text-align:left">
                  <p style="font-weight:bold">•Mazatlán</p>
                  <p>Calle Ferrusquilla s/n Interior  <br>
Edificio central de Autobuses <br>
Colonia Palos Prietos <br>
Tel: 01 (669) 981 46 59</p>
            </div>
            <div style="display:inline-block;padding:10px;text-align:left">
                  <p style="font-weight:bold">•Plaza San Ignacio</p>
                  <p>Carretera Internacional Km. 1205 <br>
Col. Fovisste en Playa Azul <br>
Tel. 01 (669) 135 12 34</p>
            </div>
            <div style="display:inline-block;padding:10px;text-align:left">
                  <p style="font-weight:bold">•Culiacán</p>
                  <p style="font-size:12px"><strong>CENTRAL DE AUTOBUSES CULIACÁN</strong> <br>
en sala de primera y sala de segunda. <br>
Blvd. Rolando Arjona # 2571 Norte y Blvd. <br>
Culiacan Colonia Desarrollo Urbano 3 Rios.  <br>
Tel. 01 (667) 761 27 61</p>
            </div>
            <div style="display:inline-block;padding:10px;text-align:left">
                  <p style="font-weight:bold">•Guamuchil</p>
                  <p>Nicolas Bravo y Adolfo Lopez Mateos # 479  <br>
                    Colonia Morelos <br>
Tel: 01 (673) 732 67 50</p>
            </div>
            <div style="display:inline-block;padding:10px;text-align:left">
                  <p style="font-weight:bold">•Guasave</p>
                  <p >Blvd. 16 de Septiembre y 5 de Febrero S/n <br>
                    Colonia <Centrobr></Centrobr>
Tel. 01 (687) 872 98 45</p>
            </div>
            <div style="display:inline-block;padding:10px;text-align:left">
                  <p style="font-weight:bold">•Los Mochis</p>
                  <p style="font-size:12px"><strong>TERMINAL</strong> <br>
Blvd. Rosendo G. Castro y <br>
Belisario Dominguez # 620 Sur <br>
Colonia Bienestar <br>
Tel: 01 (668) 817 59 07 <br>

<strong>CENTRAL DE AUTOBUSES</strong> <br>
Blvd. Rosendo G. Castro y Constitucion # 302 Oriente Col. Bienestar <br>
Tel: 01 (668) 815 67 84</p>
            </div>
            
      </div>

</div>

<!-- ====================================CENTROS COMERCIALES============================================================ -->

<div id="mostrarcentros" style="display:none">
            
            <div style="display:block;text-align:center;font-family: 'helvetica','raleway'">
               <div style="display:inline-block"><img src="img/comerciales.jpg" alt=""> </div>
               <div style="display:inline-block"><p style="font-size:12px">CENTROS COMERCIALES</p>
                <p style="color:#1959A0;font-weight:bold">SINALOA</p></div>
            </div>
            <div style="display:inline-block;font-family: 'helvetica',raleway';width:100%;text-align:center">
                  
                  <div style="display:inline-block;font-size:15px;text-align:left;padding:10px">
                        <img style="width:100px" src="img/bodega_aurrera.png" alt="">
                        <p style="font-weight:bold;">•Estadio, Culiacán</p>
                        <p>Blvd. Enrique Sáncez Alonso 2402 Nte. <br>
                            Col. Desarrollo Urbano Tres Rios  <br>
                            C.P. 80030 <br>
                            Tel. 01 (667) 146 61 60</p>
                  </div>
                  <div style="display:inline-block;font-size:15px;text-align:left;padding:10px">
                        <img style="width:100px" src="img/walmart.png" alt="">
                        <p style="font-weight:bold;"> •Los Mochis</p>
                        <p>Blvd. Jiquilpan 1112 <br>
                            Pte Local B <br>
                            Col. Jiquilpan Ahome C.P. 81220 <br>
                            Tel. 01 (668) 818 28 35</p>
                  </div>
                  <div style="display:inline-block;font-size:15px;text-align:left;padding:10px">
                        <img style="width:100px" src="img/walmart.png" alt="">
                        <p style="font-weight:bold;">•Culiacán</p>
                        <p>Av. Regional 1330 Norte <br>
                            Col. Sector V de la Primera del <br>
                            Plan Parcial <br>
                            (Tres Tios) C.P. 80000 <br>
                            Tel. 01 (667) 750 95 42</p>
                  </div>
                  <div style="display:inline-block;font-size:15px;text-align:left;padding:10px">
                        <img style="width:100px" src="img/walmart.png" alt="">
                        <p style="font-weight:bold;">•Walmart 68, Culiacán</p>
                        <p>Carretera Internacional 2017  <br>
                            Col. Hacienda del Mar C.P. 82128 <br>
                            Tel. 01 (669) 940 81 66</p>
                  </div>
                  <div style="display:inline-block;font-size:15px;text-align:left;padding:10px">
                        <img style="width:100px" src="img/walmart.png" alt="">
                        <p style="font-weight:bold;">•El Cochi, Mazatlán</p>
                        <p>Av. Manuel J. Clouthier 4459 <br>
                            Col. El Cochi C.P. 82139</p>
                  </div>
                  <div style="display:inline-block;font-size:15px;text-align:left;padding:10px">
                        <img style="width:100px" src="img/walmart.png" alt="">
                        <p style="font-weight:bold;">•Soriana Santa Rosa, Mazatlán</p>
                        <p>Av. Luis Donaldo Colosio 17301  <br>
                          Col. Fracc. Valle Dorado C.P. 82165 <br>
                          Tel. 01 (669) 940 58 34</p>
                  </div>
                                  

            </div>

</div>

<!-- ======================================AGENCIAS DE VIAJES==================================================== -->

<div id="mostraragencias" style="display:none">

            <div style="display:inline-block;text-align:left;font-family: 'helvetica','raleway'">
               <div style="display:inline-block"><img src="img/agencias.jpg" alt=""> </div>
               <div style="display:inline-block"><p style="font-size:12px">AGENCIAS DE VIAJES</p>
                <p style="color:#1959A0;font-weight:bold">SINALOA</p></div>
            </div>

            <center><table>
                  <tr>
                        <td class="municipios">MUNICIPIO</td>
                        <td class="municipios">COLONIA</td>
                        <td class="direccion">DIRECCIÓN</td>
                        <td class="municipios">TELÉFONO 1</td>
                        <td class="municipios">TELÉFONO 2</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: SERVICIOS Y VIAJES MICHEL</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">CULIACAN</td>
                        <td class="rellenotabla">ALMADA</td>
                        <td class="rellenotabla2">LAZARO CARDENAS N198-G</td>
                        <td class="rellenotabla">6677128033</td>
                        <td class="rellenotabla">6677128711</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: ROSEF & TURISMO</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">CULIACAN</td>
                        <td class="rellenotabla">CHAPULTEPEC</td>
                        <td class="rellenotabla2">ALVARO OBREGON 1330 NTE</td>
                        <td class="rellenotabla">6677127700</td>
                        <td class="rellenotabla">6677127701</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: AGENCIA DE VIAJES NERY TOURS</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">CULIACAN</td>
                        <td class="rellenotabla">FEDERALISMO</td>
                        <td class="rellenotabla2">BLVD CULIACAN 450-10</td>
                        <td class="rellenotabla">6677612992</td>
                        <td class="rellenotabla"></td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: CULHUACAN TOURS</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">CULIACAN</td>
                        <td class="rellenotabla">LAS QUINTAS</td>
                        <td class="rellenotabla2">PRESA BALSEQUILLOS N 13</td>
                        <td class="rellenotabla">6677152606</td>
                        <td class="rellenotabla">6677152603</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: SULLIVAN TURISMO</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">CULIACAN</td>
                        <td class="rellenotabla">VALLADO</td>
                        <td class="rellenotabla2">BLVD EMILIANO ZAPATA N 2151</td>
                        <td class="rellenotabla">6677178364</td>
                        <td class="rellenotabla">6677176350</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: AGENCIA MORAGA</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">GUAMUCHIL</td>
                        <td class="rellenotabla">CENTRO</td>
                        <td class="rellenotabla2">AGUSTINA RAMIREZ SUR NO. 671-A</td>
                        <td class="rellenotabla">673 7322401</td>
                        <td class="rellenotabla"></td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: VIAJES LUDACAR</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">GUASAVE</td>
                        <td class="rellenotabla">CENTRO</td>
                        <td class="rellenotabla2">EMILIANO ZAPATA N 45</td>
                        <td class="rellenotabla">6878714677</td>
                        <td class="rellenotabla">6878714657</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: SOLEITOURS</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">LOS MOCHIS</td>
                        <td class="rellenotabla">CENTRO</td>
                        <td class="rellenotabla2">CARDENAS PONIENTE N.- 509</td>
                        <td class="rellenotabla">6688123030</td>
                        <td class="rellenotabla">018009676534</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: VIAJES TRANVIA</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">LOS MOCHIS</td>
                        <td class="rellenotabla">CENTRO</td>
                        <td class="rellenotabla2">CALLEJON TENOCHTITLAN PTE N.- 399</td>
                        <td class="rellenotabla">6688123864</td>
                        <td class="rellenotabla"></td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: SERVICIOS TURISTICOS CONELVA</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">LOS MOCHIS</td>
                        <td class="rellenotabla">CENTRO AHOME</td>
                        <td class="rellenotabla2">GABRIEL LEYVA SOLANO 357 NTE</td>
                        <td class="rellenotabla">6688152484</td>
                        <td class="rellenotabla">6688158090</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: VIAJES COPALA</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">MAZATLAN</td>
                        <td class="rellenotabla">C.C LA GRAN PLAZA</td>
                        <td class="rellenotabla2">APOLO Y REFORMA L -F-4</td>
                        <td class="rellenotabla">6699862120</td>
                        <td class="rellenotabla"></td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: VIAJES MARLOPOS</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">MAZATLAN</td>
                        <td class="rellenotabla">CENTRO</td>
                        <td class="rellenotabla2">AV OLAS ALTAS N 11</td>
                        <td class="rellenotabla">6699811993</td>
                        <td class="rellenotabla">6699811994</td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: VISTA TOURS MAZATLAN S.A DE C.V</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">MAZATLAN</td>
                        <td class="rellenotabla">LOMAS DE MAZATLAN</td>
                        <td class="rellenotabla2">AV. CAMARON SABALO NO. 51 LOC. 2 Y 3</td>
                        <td class="rellenotabla">(669) 9868610</td>
                        <td class="rellenotabla"></td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: PROMOTOURS EL CID</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">MAZATLAN</td>
                        <td class="rellenotabla">ZONA DORADA</td>
                        <td class="rellenotabla2">AV CAMARON SABALO S/N LOC. 18</td>
                        <td class="rellenotabla"></td>
                        <td class="rellenotabla"></td>
                  </tr>
                  <tr>
                        <td class="textocolspan" colspan="5">Agencia: AGENCIA DE VIAJES NERY TOURS SUC NOVOLATO</td>
                  </tr>
                  <tr>
                        <td class="rellenotabla">NAVOLATO</td>
                        <td class="rellenotabla">CENTRO</td>
                        <td class="rellenotabla2">AV NIÑOS HEROES 484-A</td>
                        <td class="rellenotabla">6727271850</td>
                        <td class="rellenotabla"></td>
                  </tr>
                  
            </table></center>

</div>

<!-- =================================================OXXO======================================================================== -->

<div id="mostraroxxo" style="display:none">
      
          <p style="font-family:'helvetica','raleway'">Adquiere tus boletos de autobus TAP en las tiendas Oxxo.</p>

</div>


</div>

  <script src="//ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
  <script>
  
  $(function(){
       $('#oxxo').on('click', mostraroxo);

       function mostraroxo(){
        $('#mostraroxxo').css('display', 'block');
        $('#mostrartermi').css('display', 'none');
        $('#mostraragencias').css('display', 'none');
        $('#mostrarcentros').css('display', 'none');
       } ;
      
      $('#agencias').on('click',mostraragen);

        function mostraragen(){
        $('#mostraragencias').css('display', 'block');
        $('#mostraroxxo').css('display', 'none');
        $('#mostrartermi').css('display', 'none');
        $('#mostrarcentros').css('display', 'none');
       } ;

       $('#centros').on('click',mostrarcen);

        function mostrarcen(){
        $('#mostrarcentros').css('display', 'block');
        $('#mostraroxxo').css('display', 'none');
        $('#mostrartermi').css('display', 'none');
        $('#mostraragencias').css('display', 'none');
       } ;

       $('#terminales').on('click',mostrarter);

       function mostrarter (){
        $('#mostrartermi').css('display', 'block');
        $('#mostraroxxo').css('display', 'none');
        $('#mostraragencias').css('display', 'none');
        $('#mostrarcentros').css('display', 'none');
        

       }
      

  });


</script>
</body>
</html>