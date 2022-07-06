<?php //var_dump($games) ?>
<?php $this->load->view('includes/tables-head') ?>
<body class="fixed-header" ng-app="app" ng-controller="gameCtrl">
	<?php $this->load->view('admin/admin-nav') ?>


	<div class="page-container">

		<?php $this->load->view('admin/admin-header') ?>


		<div class="page-content-wrapper ">

			<div class="content ">

				<div class="jumbotron" data-pages="parallax">
					<div class="container-fluid container-fixed-lg sm-p-l-20 sm-p-r-20">
						<div class="inner">

							<ul class="breadcrumb">
								<li><p>Dashboard</p></li>
								<li><a href="#" class="active">Game</a></li>
								<li><a href="#" class="active">New</a></li>
							</ul>

						</div>
					</div>
				</div>


				<div class="container-fluid container-fixed-lg">


					<div class="panel">

						<ul class="nav nav-tabs nav-tabs-linetriangle" data-init-reponsive-tabs="dropdownfx">
							<li class="active">
								<a data-toggle="tab" href="#new"><span>New Game</span></a>
							</li>
							<li>
								<a data-toggle="tab" href="#game"><span>This Week Game</span></a>
							</li>
							
						</ul>

						<div class="tab-content">
							<div class="tab-pane slide-left active" id="new">
								<!-- View Branches Table start -->
								<div class="conatiner">
									<div class="row">
										<div class="col-lg-12 col-md-12">
											<div class="clearfix"></div>
											<?php echo validation_errors(); ?>
											<table class="table">
												<thead>
													<tr>
														<td>NUMBER</td>
														<td>HOME</td>
														<td>AWAY</td>
														<td>DEADLINE <small>format: (2016-04-18 11:24:00)</small></td>
														
													</tr>

												</thead>
												<tbody>
													<form role="form" method="post" action="<?=site_url('admin/game/create') ?>">
														<?php for($i = 1; $i<=49; $i++){ ?>
														<tr>

															<td>
																<?=$i; ?>
																<input type="hidden" name="data[<?=$i; ?>][number]" value="<?=$i; ?>" />
															</td>

															<td>
																<input id="home" type="text" name="data[<?=$i ?>][home]" class="form-control" required />
															</td>
															<td>
																<input id="away" type="text" name="data[<?=$i ?>][away]" class="form-control" required />
															</td>
															<td>
																<input type="date" class="input-sm form-control" name="data[<?=$i ?>][deadline]" placeholder="2016-04-18 11:24:00" required/>
															</td>

														</tr>
														<?php } ?>	
														<?php  $csrf = array(
															'name' => $this->security->get_csrf_token_name(),
															'hash' => $this->security->get_csrf_hash());
															?>
															<input type="hidden" name="<?=$csrf['name'];?>" value="<?=$csrf['hash'];?>" />
															<tr>
																<td>
																	<label for="number">Week Number <small>e.g. 34</small></label>
																	<input type="number" name="week_number" class="form-control" required placeholder="0">
																</td>

																<td>
																	<label for="week_start_date">Week Start Date (<small>format: 2016-04-18</small>)</label>
																	<input  type="date" name="week_start_date" class="form-control" required placeholder="2016-04-18">
																</td>
																<td>
																	<label for="week_start_date">Week End Date (<small>format: 2016-04-18</small>)</label>
																	<input  type="date" name="week_end_date" class="form-control" required placeholder="2016-04-18">
																</td>
																<td>
																	<label for="submit">Submit All Entries</label><br />
																	<input id="sumbit" type="submit" name="submit" value="Submit" class="btn btn-sm btn-primary">
																</td>
															</tr>
															
														</form>	 								
													</tbody>

												</table>
												<br />
											</div>

										</div>
									</div>
									<!-- Veiw Branches Table end -->
								</div>

								<div class="tab-pane slide-left" id="game">
									<!-- View Branches Table start -->
									<div class="conatiner">
										<div class="row">
											<div class="col-lg-12 col-md-12">

												<div class="clearfix"></div>

												<table class="table table-responsive">
													<thead>
														<tr>
															<td>NUMBER</td>
															<td >DEADLINE</td>
															<td>TOGGLE</td>
														</tr>

													</thead>
													<?php if($games): ?>
													<tbody>
														<?php foreach ($games as $row): ?>
															<tr>
																<td><?=$row->number; ?></td>
																<td><?=$row->deadline ?></td>
																<td>
																	<?php if ($row->status == 'active'){ ?>
																	<input type="checkbox" ng-click="toggleGame(<?=$row->number; ?>, <?=$row->week_number ?>)" id="<?=$row->number ?>"checked/>
																	<?php  }else{ ?>
																	<input type="checkbox" disabled id="<?=$row->number ?>" />
																	<?php } ?>
																</td>
															</tr>
														<?php endforeach; ?>										
													</tbody>
												<?php endif; ?>
												</table>
											</div>

										</div>
									</div>
									<!-- Veiw Branches Table end -->
								</div>

							</div>



						</div>

					</div>

					<?php $this->load->view('includes/footer-note') ?>

				</div>

			</div>
			<?php $this->load->view('includes/tables-footer') ?>