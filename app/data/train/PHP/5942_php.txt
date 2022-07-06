<?php
require_once 'classes/DaoMercadoria.php';
require_once 'classes/DaoTipoNegocio.php';
include 'cabecalho.php';
 ?>
 <div class="well">
     <h1 class="text-center">Plataforma de Negocios</h1>
   </div>
 </header>
  <section>
  <?php
    $mercadoria = new DaoMercadoria();
    if (isset($_POST['alterar'])) {
    $codigo_id = $_POST['codigo_id'];
    $mercadorias = $mercadoria->findUnit($codigo_id);
    foreach ($mercadorias as $mercadoria):?>
  <main class="container">
    <legend class="text-center">Alteração das Informações</legend>
    <form action="alterarMercadoria.php" method="post" id="formAlterar"  name="formAlterar">
      <div class="form-group">
          <label for="">Código da mercadoria:</label>
          <input type="number" class="form-control" id="codigo_id" name="codigo_id"  value="<?=$mercadoria->codigo_id?>">
      </div>
      <div class="form-group">
          <label for="">Tipo da Mercadoria:</label>
          <input type="text" class="form-control" id="tipo" name="tipo" value="<?=$mercadoria->tipo?>">
      </div>
      <div class="form-group">
          <label for="">Nome da mercadoria:</label>
          <input type="text" class="form-control" id="nome" name="nome" value="<?=$mercadoria->nome?>">
      </div>
      <div class="form-group">
          <label for="">Quantidade:</label>
          <input type="number" class="form-control" id="quantidade" name="quantidade" value="<?=$mercadoria->quantidade?>">
      </div>
      <div class="form-group">
          <label for="">Preço:</label>
          <input type="number" class="form-control" id="preco" name="preco" value="<?=$mercadoria->preco?>">
      </div>
      <div class="form-group">
            <label for="">Tipo do Negocio:</label>
            <select  class="form-control" id="tipoNegocio" name="tipoNegocio">
                <?php
                  $negocio = new DaoTipoNegocio();
                  $negocios = $negocio->findALL();
                  foreach ($negocios as $negocio ) {
                    $Selecionada = $mercadoria->tiponegocio_id == $negocio->tiponegocio_id;
                    $selecao = $Selecionada ? "selected='selected'" : "";
                    ?>
                    <option  value="<?=$negocio->tiponegocio_id?>" <?=$selecao?> ><?=$negocio->nome?></option>
                <?php }
                 ?>
               </select>
               <button type="submit" class="btn default" name="confirma" >Alterar</button>
        </div>
    <?php endforeach;
  }else{
    echo "nada";
  }
     ?>
    </form>
  </main>
</section>
<?php
include 'rodape.php';
 ?>
