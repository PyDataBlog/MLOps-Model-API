# ansibleCVSworkflow

This tool can commit and update specified file to CVS Repository.
Optional feature: run command after update

Examples:
- Run command after cvs update in groupname group
```
  ansible-playbook --extra-vars "file=named.conf servers=groupname destination=/etc/bind cvsdir=~/cvsworkdir command='ls -all'" site.yml
```
- Only update remote files in hist1, host2
```
  ansible-playbook --extra-vars "file=named.conf servers=host1;host2 destination=/etc/bind cvsdir=~/cvsworkdir" site.yml
```
Where:
  - file: filename on local (this will be copied to local CVS for commit and update on remote)
  - servers: server or group names
  - destination: directory on remote where we want to update the file
  - command: command with arguments

Test opportunities:
```
  --skip-tags "cvscommit,cvsupdate,cvslocalupdate"
```
