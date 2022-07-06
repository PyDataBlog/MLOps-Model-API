<?php defined('BASEPATH') OR exit('No direct script access allowed'); ?>

     <article class="col-md-8" id="main" >
        <div class="col-md-12 scroll" id="content"> 
        <div id="crumbs">Dashboard <i class="fa fa-chevron-right"></i> Users</div>
            <div class="row">
			<div class="col-md-6" id="c__u">
				<h5 class="title">Add Users</h5>
				<div class=" col-centered">
  			<?php echo form_open('users/add'); ?>
<?php if (isset($message)) { ?>
<CENTER><h3 style="color:green;">Data inserted successfully</h3></CENTER><br>
<?php } ?>
<?php echo form_error('level'); ?><br />
		<table border="0" width="100%" align="center">

			<tr>
				<td><span class="select-style">	
					<select name="level">
							    <?php       //if there is comments then print the comments
				                if(count($levels) > 0)
				                {
				                    foreach ($levels as $level)
				                    {

				                 ?>
				         <option value="<?=$level['level_id'];?>">
					                    <?=$level['name'];?>
					     </option>
				            <?php   }
				                }
				                else //when there is no comment
				                {
				                    echo "<p>No Levels</p>";
				                }
				                 ?>
							</select>​
				</span></td>
				<td>
				<span class="select-style">
					<select name="utility_id">
					 	    <option>Select Commecial Utility</option>
							    <?php       //if there is comments then print the comments
				                if(count($utilities) > 0)
				                {
				                    foreach ($utilities as $utility)
				                    {

				                 ?>
				         <option value="<?=$utility['cu_id'];?>">
					                    <?=$utility['utility'];?>
					     </option>
				            <?php   }
				                }
				                else //when there is no comment
				                {
				                    echo "<p>No Utilities</p>";
				                }
				                 ?>
					</select>​
				</span>
				</td>
				</tr>
	</table>
<?php echo form_error('fname'); ?><br />
<input type="text" name="fname"  id="fname" value="" />

<?php echo form_error('lname'); ?><br />
<input type="text" name="lname"  id="lname" value="" />

 <?php echo form_error('email'); ?><br />
<?php echo form_input(array('id' => 'email', 'name' => 'email')); ?><br />

<?php echo form_error('password'); ?><br />
<input type="text" name="password"  id="password" value="<?php echo get_random_password(); ?>" />

<?php echo form_submit(array('id' => 'submit', 'value' => 'Submit')); ?>
<?php echo form_close(); ?><br/>
<div id="fugo">

</div>
					</div>
					</div>


				</div>
			</div> 
    </article>
