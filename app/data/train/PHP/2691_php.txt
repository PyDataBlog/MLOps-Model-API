<div class="content">
			<div class="categories">
				<div class="container">
					<?php foreach ($categories as $category):?>
					<div class="col-md-2 focus-grid">
						<a href="<?php echo base_url()."index.php/middleware/categories/".$this->encrypt->encode($category->ProductCategoryID)?>">
							<div class="focus-border">
								<div class="focus-layout">
									<div class="focus-image"><i class="<?php echo $category->Fa?>"></i></div>
									<h4 class="clrchg"><?php echo $category->Name?></h4>
								</div>
							</div>
						</a>
					</div>
					<?php endforeach;?>
					<div class="clearfix"></div>
				</div>
			</div>
			