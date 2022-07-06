<body>

<div id="container_guiga">
	<h1 class="h1_guiga">	Bem-vindo!	</h1>

	<div id="body_guiga">
		<?php
			echo "hi";
			echo form_open('usuario/login');
			
				$inputEmail = array('name'=>'email',
									'id'=>'email',
									'style'=>'width:40%;font-size:100px');
			
				$inputSenha = array('name'=>'senha',
									'id'=>'senha',
									'maxlength'=>'10',
									'style'=>'width:40%;font-size:100px');
			
			echo form_input($inputEmail);
			
			echo form_password($inputSenha);
			
			
			echo form_close();
			
			//<button type="submit" style="width:150px;height:75px" class="btn btn-default">Logar!</button>
			/*echo "<br>";
			$img_delete = array(
					'src' => 'assets/img/delete_32x32.png',
					'alt' => 'Delete'
			);
			$img_detalhes = array(
					'src' => 'assets/img/detalhes_32x32.png',
					'alt' => 'Detalhes'
			);
			$img_editar = array(
					'src' => 'assets/img/editar_32x32.png',
					'alt' => 'Editar'
			);		
			$this->table->set_heading(array('Nome','Ver','Editar','Excluir'));
			foreach ($usuarios as $usuario){
				$this->table->add_row(array(
						$usuario->username,
						anchor('welcome', img($img_delete)),
						anchor('welcome', img($img_detalhes)),
						anchor('welcome', img($img_editar))
						));
			}
			echo $this->table->generate();
		*/
		?>
		<h2>Hi </h2>
		</div>
</body>		
<!-- 		<table class="table-striped" > -->
<!-- 		<tr> -->
<!-- 			<td>Teste</td> -->
<!-- 			<td>Teste</td> -->
<!-- 		</tr> -->
<!-- 		<tr> -->
<!-- 			<td>Teste</td> -->
<!-- 			<td>Teste</td> -->
<!-- 		</tr>		 -->
<!-- 		</table> -->
	