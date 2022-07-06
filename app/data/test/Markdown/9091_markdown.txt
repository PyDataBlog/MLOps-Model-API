Ansible Role: s3 vars
=========

As an alternative to using the ansible vault, this role delegates to localhost to retrieve a config file - expected to be in either YAML or JSON format - from an s3 bucket and converts the file to playbook vars.

Requirements
------------

* boto
* caller needs credentials with read access on bucket and config file being requested.  Credentials are retrieved from a specified AWS profile, or we fall back to environment variables as detailed in the s3 ansible module (http://docs.ansible.com/ansible/s3_module.html).

Role Variables
--------------

* s3_vars_file_dest: where s3 file is stored locally, defaults to /tmp
* s3_vars_bucket: the s3 bucket where yaml/json file is located
* s3_vars_object: path/to/YAML/OR/JSON/file in bucket
* s3_vars_aws_profile: the AWS profile used by play caller as credentials to read from the target bucket, omitted by default
* s3_vars_config_local_file_name: destination file name of config file on local machine.  Defaults to dash-delimited combination of bucket and object, where object folder references are converted to dashes.
* s3_vars_config_cleanup: whether to remove the config file after including it, defaults to true

Dependencies
------------

N/A

Example Playbook
----------------

Including an example of how to use your role (for instance, with variables passed in as parameters) is always nice for users too:

    - hosts: servers
      roles:
         - { role: ansible-role-s3-vars, s3_vars_bucket: app-secrets, s3_vars_object: path/to/config.yml }

License
-------
MIT
