oauth2-proxy
============

[![Build Status](https://img.shields.io/travis/marvinpinto/ansible-role-oauth2-proxy/master.svg?style=flat-square)](https://travis-ci.org/marvinpinto/ansible-role-oauth2-proxy)
[![Ansible Galaxy](https://img.shields.io/badge/ansible--galaxy-oauth2--proxy-blue.svg?style=flat-square)](https://galaxy.ansible.com/marvinpinto/oauth2-proxy)
[![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat-square)](LICENSE.txt)

Ansible Galaxy role to install and manage [OAuth2 Proxy](https://github.com/bitly/oauth2_proxy).


Requirements
------------

This role has been tested on Ubuntu 14.04 and will likely only work on an
Ubuntu-like system.


Role Variables
--------------

``` yaml
# Corresponds to the GitHub releases
# https://github.com/bitly/oauth2_proxy/releases
oauth2_proxy_version: '2.1'

# User to run the OAuth2 Proxy daemon as
oauth2_proxy_daemon_user: 'oauth2daemon'

# OAuth2 Proxy cli arguments
oauth2_proxy_cli_args: >-
  --upstream="http://127.0.0.1:8080/"
  --cookie-secret="muchsekr3t"
  --client-id="doge"
  --client-secret="suchsekuR3"
  --email-domain=*
```


Examples
--------

Install this module from Ansible Galaxy into the './roles' directory:
```bash
ansible-galaxy install marvinpinto.oauth2-proxy -p ./roles
```

Use it in a playbook as follows:
```yaml
- hosts: '127.0.0.1'
  roles:
    - role: 'marvinpinto.oauth2-proxy'
      become: true
```


Development
-----------
Use the supplied `Vagrantfile` for local development and testing (hint: `vagrant up --provision`)
