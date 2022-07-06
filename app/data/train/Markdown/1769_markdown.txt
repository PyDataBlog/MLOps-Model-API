# consul-template-install

Installs [Hashicorp's Consul Template] (https://github.com/hashicorp/consul-template)



## Role Variables
```yml

---
  ---
  consul_template_app:
    name: consul_template
    version: 0.11.0
    arch: linux_amd64
    file_owner: consul
    file_group: consul

  consul_template_install:
    dir : /usr/local/bin
    download_location: https://github.com/hashicorp/consul-template/releases/download

    file_list:
      - consul-template

```


Example Playbook
----------------
```yml


- hosts: local
  sudo: true
  gather_facts: true

  roles:
    - { role: marcelocorreia.consul_template-install }

- vars:
    consul_template_app:
      name: consul_template
      version: 0.11.0
      arch: linux_amd64
      file_owner: consul
      file_group: consul

    consul_template_install:
      dir : /usr/local/bin
      download_location: https://github.com/hashicorp/consul-template/releases/download

      file_list:
        - consul-template

```

License
-------

MIT

Author Information
------------------
Some useful stuff at:
  - [https://github.com/marcelocorreia](https://github.com/marcelocorreia)
  - [https://galaxy.ansible.com/list#/users/15516](https://galaxy.ansible.com/list#/users/15516)
  - [https://hub.docker.com/u/marcelocorreia/](https://hub.docker.com/u/marcelocorreia/)
  - Any issues, pull-request are welcome
