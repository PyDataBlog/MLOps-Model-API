<?php

/**
 * Write users to file
 * @param string $filename
 * @param array $array_data
 * @return null
 */

// function insert($user,$config)


function readUser($id, $config)
{
	switch ($config['adapter'])
	{
		case 'txt':
			include_once('file/users.php');
			$user=readUserFromFile($id, $config);
			return $users;
			break;
		case 'mysql':
			include_once('mysql/users.php');
			$users = select($id,$config);
			return $users;
			break;
		case 'googledocs':
			include_once('googledocs/users.php');
			$users = selectAll($config);
			return $users;
			break;
	
	}
}

function update($user, $id, $config)
{
	switch ($config['adapter'])
	{
		case 'txt':
			include_once('file/users.php');
			updateUserTofile($id, $user, $user['filename'], $config);
			return;
			break;
		case 'mysql':
			include_once('mysql/users.php');
			$users = updateUser($id, $user, $config);
			return $users;
			break;
		case 'googledocs':
			include_once('googledocs/users.php');
			$users = selectAll($config);
			return $users;
			break;
	
	}
}

// function select($id, $config)

function readUsers($config)
{
	switch ($config['adapter'])
	{
		case 'txt':
			include_once('file/users.php');
			$users=readUsersFromFile($config);
			return $users;
		break;
		case 'mysql':
			include_once('mysql/users.php');
			$users = selectAll($config);
			return $users;
		break;
		case 'googledocs':
			include_once('googledocs/users.php');
			$users = selectAll($config);
			return $users;
		break;
	
	}
}

// function delete($id, $config)










