# it394_project_template
A project template for a Python/Django project ready to deply into the Heroku production environment.

# How to use this template
To use this template start a new project using django-admin and the --template parameter
```
django-admin startproject --template=http://github.com/usma-it394/it394_project_template/archive/master.zip <project name>
```
django-admin startproject will correctly rename the project_name folder and also replace the {{ }} patterns with the correct values for the contents of the files in the project_name folder.

Unfortunately, it does not apply changes to the contents of files in the root directory of the project.  I've included a simple PowerShell script named "fix_name.ps1" that will replace the HW3 pattern in any files in the current working directory.  So after you start the project run
```
cd <project name>
.\fix_name.ps1 -name "project name"
```

For example, if I wanted to start a new project named "nifty" I would enter the following:
```
django-admin startproject --template=http://github.com/usma-it394/it394_project_template/archive/master.zip nifty
cd nifty
.\fix_name.ps1 -name "nifty"
```
# References
The standard django-admin templates for projects and apps are at https://github.com/django/django/tree/master/django/conf


