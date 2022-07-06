<html>
	<head>
		<title> <?php echo $title; ?> </title>
	</head>
	<body>
		
	<?php echo form_open('dashboard/insert_user'); ?>
	<p>
		<label for="username">Username</label>
		<input type="text" id="username" name="username"> </type>
	</p>
	<p>
		<label for="lastname">Lastname</label>
		<input type="text" id="lastname" name="lastname"> </type>
	</p>
	<p>
		<label for="firstname">Firstname</label>
		<input type="text" id="firstname" name="firstname"> </type>
	</p>
	<p>
		<label for="password">Password</label>
		<input type="text" id="password" name="password"> </type>
	</p>
	<p>
		<label for="gender">Gender</label>
		<input type="text" id="gender" name="gender"> </type>
	</p>
	<p>
		<label for="type">type</label>
		<input type="text" id="type" name="type"> </type>
	</p>
	<p>
		<label for="email">email</label>
		<input type="text" id="email" name="email"> </type>
	</p>
	<p>
		<input type="submit" value="Submit">
	</p>

	<?php echo form_close();?>
		
		
	</body>
</html>