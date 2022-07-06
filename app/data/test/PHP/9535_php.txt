<div id="page-wrapper">
	<div class="page-wrapper">
		<div class="row">
			<div class="col-xs-12">
				<h1>Accueil</h1>

				<?php if (isset($_SESSION) && $this->session->userdata('is_logged_in') != NULL && $this->session->userdata('user_info')->is_confirmed == 1 ) : ?>
					<h2>Bonjour <?php echo $this->session->userdata['user_info']->prenom; ?></h2>
				<?php endif; ?>

				<?php if (isset($_SESSION) && $this->session->userdata('user_info')->is_confirmed == 0) : ?>
					<h2>Votre compte n'a pas été activé.<br>Veuillez consulter vos courriels ( <?php echo $this->session->userdata('user_info')->courriel; ?> ) pour activer votre compte.</h2>
					<h3><a href="<?php echo site_url('user/resendValidateEmail/') .$this->session->userdata('user_info')->courriel; ?>"><span class="fa fa-paper-plane"></span> Renvoyer le courriel</a></h3>
					<h4>Si votre adresse courriel est inexacte, veuillez communiquer avec un organisateur au <em>admin@enfantsdecain.ca</em></h4>
				<?php endif; ?>

				<?php if (isset($userInfo) && $userInfo == false) : ?>
					<h2>Erreur de connexion</h2>
				<?php endif; ?>

			</div>
		</div>
		<div class="row">
			<div class="col-md-6 col-xs-12">
				<?php 
					if (isset($news) && isset($this->session->userdata['user_info']) && $this->session->userdata('is_logged_in') == 1): 
						foreach ($news as $new) { 
				?>
						<div class="panel panel-default">
							<div class="panel-heading">
							<?php 
								if( $new->modified ) {
									$date = 'Modifiée le ' . date('j M Y',$new->modified); 
								} else {
									$date = date('j M Y',$new->date);
								}
							?>
								<h4><?php echo $new->title; ?><div style="float: right;display:inline;"><?php echo $date; ?>
									<?php if ($this->session->userdata['user_info']->is_admin) : ?>
										<a href="<?php echo base_url(); ?>news/readSingleNews/<?php echo $new->id; ?>"><button class="btn btn-primary"><span class="fa fa-edit"></span></button></a>
										<a class="btn-confirmCancel" data-cancel="<?php echo $new->id; ?>" href="#"><button class="btn btn-danger"><span class="fa fa-remove"></span></button></a>
									<?php endif; ?>
								</div></h4>
							</div>
							<div class="panel-body">
								<em>Par <?php echo $new->prenom . ' ' .$new->nom; ?></em>
								<p><?php echo $new->content ?></p>
							</div>
						</div>
						
				<?php 
						} //endForeach
					endif; 
				?>
			</div>
		</div>
	</div>
</div>