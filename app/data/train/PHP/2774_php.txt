<link rel="stylesheet" type="text/css" href="<?php echo base_url(); ?>js/sweetalert2.css">
<script src="<?php echo base_url(); ?>js/sweetalert2.min.js"></script>
<script>
    function hapusData(education_id) {
        var id = education_id;
        swal({
            title: 'Are You Sure ?',
            text: 'This Data will be Deleted !',type: 'warning',
            showCancelButton: true,
            confirmButtonColor: '#3085d6',
            cancelButtonColor: '#d33',
            confirmButtonText: 'Yes',
            cancelButtonText: 'No',
            closeOnConfirm: true
        }, function() {            
            window.location.href="<?php echo site_url('master/education/deletedata'); ?>"+"/"+id
        });
    }
</script>

<script type="text/javascript">
	$(function() {
		$(document).on("click",'.edit_button', function(e) {
	        var name = $(this).data('name');
	        var id   = $(this).data('id');
	        //console.log(name, id)
	        $(".education_id").val(id);
	        $(".education_name").val(name);
	    })
	});
</script>

<div class="page-container">
	<!-- BEGIN PAGE HEAD -->
	<div class="page-head">
		<div class="container">
			<!-- BEGIN PAGE TITLE -->
			<div class="page-title">
				<h1>Employee <small>Master Data</small></h1>
			</div>
			<!-- END PAGE TITLE -->				
		</div>
	</div>
	<!-- END PAGE HEAD -->
	<!-- BEGIN PAGE CONTENT -->
	<div class="page-content">
		<div class="container">
			<!-- BEGIN SAMPLE PORTLET CONFIGURATION MODAL FORM-->
			 <div class="modal fade" id="add" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
				<div class="modal-dialog">
					<div class="modal-content">
					<form action="<?php echo site_url('master/education/savedata'); ?>" class="form-horizontal" method="post">
					<input type="hidden" name="<?php echo $this->security->get_csrf_token_name(); ?>" value="<?php echo $this->security->get_csrf_hash(); ?>">
						
						<div class="modal-header">						
							<button type="button" class="close" data-dismiss="modal" aria-hidden="true"></button>
							<h4 class="modal-title"><i class="fa fa-plus-circle"></i> Form Add Education</h4>
						</div>
						<div class="modal-body">
							 <div class="form-group">
								<label class="col-md-3 control-label">Education Name</label>
								<div class="col-md-8 has-error">
									<input type="text" class="form-control" placeholder="Enter Education Name" name="name" autocomplete="off" required autofocus>
								</div>
							</div>
						</div>
						
						<div class="modal-footer">
							<button type="submit" class="btn btn-primary"><span class="glyphicon glyphicon-floppy-disk"></span> Save</button>
							<button type="button" class="btn btn-default" data-dismiss="modal"><span class="glyphicon glyphicon-chevron-left"></span> Close</button>
						</div>
					</form>
					</div>
					<!-- /.modal-content -->
				</div>
				<!-- /.modal-dialog -->
			</div>
			<!-- /.modal -->
			<!-- Edit -->
			<div class="modal fade" id="edit" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
				<div class="modal-dialog">
					<div class="modal-content">
					<form action="<?php echo site_url('master/education/updatedata'); ?>" class="form-horizontal" method="post">
					<input type="hidden" name="<?php echo $this->security->get_csrf_token_name(); ?>" value="<?php echo $this->security->get_csrf_hash(); ?>">
					<input type="hidden" class="form-control education_id" name="id">
						
						<div class="modal-header">						
							<button type="button" class="close" data-dismiss="modal" aria-hidden="true"></button>
							<h4 class="modal-title"><i class="fa fa-edit"></i> Form Edit Education</h4>
						</div>
						<div class="modal-body">
							<div class="form-group">
								<label class="col-md-3 control-label">Education ID</label>
								<div class="col-md-2 has-error">
									<input type="text" class="form-control education_id" placeholder="" name="id" autocomplete="off" readonly>
								</div>
							</div>
							<div class="form-group">
								<label class="col-md-3 control-label">Education Name</label>
								<div class="col-md-8 has-error">
									<input type="text" class="form-control education_name" placeholder="Enter Education Name" name="name" autocomplete="off" required autofocus>									
								</div>
							</div>
						</div>
						
						<div class="modal-footer">
							<button type="submit" class="btn btn-primary"><span class="glyphicon glyphicon-floppy-disk"></span> Update</button>
							<button type="button" class="btn btn-default" data-dismiss="modal"><span class="glyphicon glyphicon-chevron-left"></span> Close</button>
						</div>
					</form>
					</div>
					<!-- /.modal-content -->
				</div>
				<!-- /.modal-dialog -->
			</div>

			<!-- BEGIN PAGE BREADCRUMB -->
			<ul class="page-breadcrumb breadcrumb">
				<li>
					<a href="<?php echo base_url(); ?>">Home</a><i class="fa fa-circle"></i>
				</li>
				<li>
					<a href="#">Employee</a>
					<i class="fa fa-circle"></i>
				</li>
				<li>
					<a href="#">Master Data</a>
					<i class="fa fa-circle"></i>
				</li>
				<li class="active">
					 Education
				</li>
			</ul>
			<!-- END PAGE BREADCRUMB -->

			<!-- BEGIN PAGE CONTENT INNER -->
			<div class="row">			
				<div class="col-md-12">
					<!-- BEGIN EXAMPLE TABLE PORTLET-->
					<div class="portlet light">
						<div class="portlet-title">
							<div class="caption">
								<i class="fa fa-folder font-blue-sharp"></i>
								<span class="caption-subject font-blue-sharp bold uppercase">Education List</span>
							</div>							
							<div class="tools">
							</div>
						</div>						
						<a data-toggle="modal" href="#add">
							<button type="submit" class="btn btn-primary">
							<i class="icon-plus"></i> Add Data</button>
						</a>
						<?php 
				        if ($this->session->flashdata('notification')) { ?>
				        	<script>
		                        swal({
		                            title: "Done",
		                            text: "<?php echo $this->session->flashdata('notification'); ?>",
		                            timer: 2000,
		                            showConfirmButton: false,
		                            type: 'success'
		                        });
		                    </script>
				        <? } ?>
						<div class="portlet-body">
							<table class="table table-striped table-bordered table-hover" id="sample_1">
							<thead>
							<tr>
								<th>
									 Education Name
								</th>
								<th width="15%">
									 Action
								</th>							
							</tr>
							</thead>
							<tbody>
							<?php 
								foreach($daftarlist as $r) { 
									$education_id = $r->education_id;
							?>
							<tr>
								<td>
									 <?php echo $r->education_name; ?>
								</td>
								<td>
									<button type="button" class="btn btn-primary btn-xs edit_button" data-toggle="modal" data-target="#edit" data-id="<?php echo $r->education_id; ?>" data-name="<?php echo $r->education_name; ?>"><i class="icon-pencil"></i> Edit 
									</button>
	                        		<a onclick="hapusData(<?php echo $education_id; ?>)"><button class="btn btn-danger btn-xs" title="Delete Data"><i class="icon-trash"></i> Delete</button></a>
								</td>							
							</tr>
							<?php } ?>						
							</tbody>
							</table>
						</div>
					</div>
					<!-- END EXAMPLE TABLE PORTLET-->				
				</div>
			</div>
			<!-- END PAGE CONTENT INNER -->
		</div>
	</div>
	<!-- END PAGE CONTENT -->
</div>