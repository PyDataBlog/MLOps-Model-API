<?php include("inc_header.php"); ?>
<?php include("inc_lang_detect.php"); ?>

				<!-- Inner Page 4404 Error -->
				<div class="container">
					<div class="inner-error">
						<h3><strong>HTTP 404<strong></h3>
						<?php if (getDefaultLanguage() == "pt-br") { ?>
							<h3 >Objeto n&atilde;o encontrado!</h3>
							<p>A URL requisitada n&atilde;o foi encontrada neste servidor.</p>
							<p>Se o endere&ccedil;o (URL) foi digitado manualmente, pedimos a gentileza de verificar novamente a sintaxe do endere&ccedil;o.</p>
						<?php } else { ?>
							<h3 >Object not found!</h3>
							<p>The requested URL was not found on this server.</p>
							<p>If you entered the URL manually please check your spelling and try again.</p>
						<?php } ?>
					</div>
				</div>
				
			</div>
			<!-- Inner Page Content End -->			
<?php include("inc_footer.php"); ?>
