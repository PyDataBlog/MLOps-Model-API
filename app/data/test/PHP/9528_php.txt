<!DOCTYPE html>
<html lang="en">

	<head>
		<meta charset="utf-8">
		<meta http-equiv="X-UA-Compatible" content="IE=edge">
		<meta name="viewport" content="width=device-width, initial-scale=1">
		<meta name="description" content="">
		<meta name="author" content="">

		<title><?php echo $this->config->item('site_name')?> - Add new post</title>

		<!-- Load CSS -->
		<?php $this->view('dashboard/_css')?>

	</head>

	<body>

		<div id="wrapper">

		<!-- Navigation -->
		<?php $this->view('dashboard/_menu')?>
		
			<div id="page-wrapper">
				<div class="row">
					<div class="col-lg-12">
						<h1 class="page-header">Add new post</h1>
					</div>
					<!-- /.col-lg-12 -->
				</div>
				<!-- /.row -->
				
				<div class="row">
					<div class="col-lg-12">
						<?php echo form_open("dashboard/posts/create");?>
							<div class="form-group">
								<label>Post Title</label>
								<input class="form-control" name="title" value="<?php echo set_value('title'); ?>" placeholder="E.g. My blog post no 1">
								<p class="help-block"><?php echo form_error('title'); ?></p>
							</div>
							<div class="form-group">
								<label>Category</label>
								<?php if(! Category::all()->isEmpty()): foreach(Category::orderBy('name')->get() as $cat):?>
								<div class="checkbox">
									<label>
										<input type="checkbox" name="category[]" value="<?php echo $cat->id?>"><?php echo $cat->name?>
									</label>
								</div>
								<?php endforeach; else:?>
								<div class="checkbox">
									<label>
										No category yet. <a class="btn btn-info btn-xs" href="<?php echo site_url('dashboard/categories/create')?>"><span class="glyphicon glyphicon-plus-sign" aria-hidden="true"></span>  Add new category</a>
									</label>
								</div>
								<?php endif;?>
								<div class="form-group">
									<label>Content</label>
									<textarea class="form-control" id="content" name="content" rows="5"><?php echo set_value('content'); ?></textarea>
									<p class="help-block"><?php echo form_error('content'); ?></p>
								</div>
								<button type="submit" class="btn btn-primary">Submit</button>
							</div>
						<?php echo form_close()?>
					</div>
				</div>
		
			</div>
			<!-- /#page-wrapper -->

		</div>
		<!-- /#wrapper -->

		<!-- Load JS -->
		<?php $this->view('dashboard/_js')?>
		<script src="//tinymce.cachefly.net/4.2/tinymce.min.js"></script>
		<script type="text/javascript">
			$(function() {
				tinymce.init({
					selector: "#content",
					plugins: "image,link",
					toolbar: [
						"undo redo | bold italic | link | image | alignleft aligncenter alignright",
						
					]
				});
			});
		</script>

	</body>
</html>