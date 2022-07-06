# Install the Latest WordPress via Applescript

## Note:
There are a few assumptions here:
* You are on a Mac
* You are using the local Apache/PHP environment
* You've set up your local Apache/PHP environment to run from your `user/Sites` directory
* You are using a local, standalone and complete install of MySQL

You shouldn't need to change much in the script if these assumptions are correct.

## Version:

v 1.0

## Setup:
* Open `install_default_wp.scpt` in Script Editor (Applications > Utilities > Script Editor)
* Edit the first two lines replacing the defaults with your local MySQL user and password
* Save the file and then go to File > Export&hellip;
* Change the "File Format" field to "Application"
* Check the "Run-only" box
* Save the file

## Advanced Setup:
* If you have MySQL in a custom location you may need to change the `db_path` variable as well. Defaults to `/usr/local/mysql/bin/mysql`
* You may also need to update `local_url` if you're running an atypical setup. Defaults to `http://localhost/`

## Use:
* Run the exported application file
* If you want to use a name other than the default "wordpress" for the folder, change it in the dialog box. (Remember this is used as a folder and database name so keep it simple and machine readable. e.g. lowercase and hyphenated or underscored)
* You'll likely be prompted for your password. This is for modifying file ownership and permissions
* Once the script has run you'll be presented with the WordPress setup window. Go through the process as per usual. Inserting your database information with the table being the same as the information you entered into the dialog window and your systems user and password. The rest of the process is up to you. Should be easy as permissions are set in the script as well.

## The Code:
```
set db_user to "user"
set db_pass to "password"
set db_path to "/usr/local/mysql/bin/mysql"
set local_url to "http://localhost/"
set wp_url to "https://wordpress.org/"
set latest_wp to "latest.tar.gz"
set posix_path to POSIX path of (path to sites folder from user domain) as text

display dialog "New site folder name (e.g. custom-name):" default answer "wordpress" buttons {"Install", "Cancel"} default button 1

if the button returned of the result is "Cancel" then
	display dialog "Cancelled"
else
	set folder_name to (text returned of the result)
end if

set curl_command to "curl " & wp_url & latest_wp & " -o " & posix_path & latest_wp
do shell script curl_command

set tar_command to "tar -xvzf " & posix_path & latest_wp & " -C " & posix_path
do shell script tar_command

set delete_command to "rm " & posix_path & latest_wp
do shell script delete_command

if folder_name is not "wordpress" then
	set rename_command to "mv " & posix_path & "wordpress " & posix_path & folder_name
	do shell script rename_command
end if

set own_command to "chown -R _www " & posix_path & folder_name
do shell script own_command with administrator privileges

set mod_command to "chmod -R g+w " & posix_path & folder_name
do shell script mod_command with administrator privileges

set database_command to db_path & " -u " & db_user & " -p" & db_pass & " -e 'create database `" & folder_name & "`;'"
do shell script database_command

set user_name to short user name of (system info)
open location local_url & "~" & user_name & "/" & folder_name
```

## History
* v 1.0 - Initial version