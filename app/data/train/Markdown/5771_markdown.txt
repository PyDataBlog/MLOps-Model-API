# Download 

## CentOS 7.1 (1503)
Download the CentOS 7.1 (1503) as following links :

### UI in most case
* http://vault.centos.org/7.1.1503/isos/x86_64/CentOS-7-x86_64-DVD-1503-01.iso

### Non User Interface - console
* http://vault.centos.org/7.1.1503/isos/x86_64/CentOS-7-x86_64-Minimal-1503-01.iso


# Minimal VirtualBox Configuration

Skip if one wants to install CentOS on the physical machine

### New > Select **Linux** and **Red Hat (64bit)**, and Give the relevant name

![Connection Example](0.png)

### The minimum recommended memory size is 2GB

![Connection Example](1.png)

### Create Hard Disk 

![Connection Example](2.png)
![Connection Example](3.png)
![Connection Example](4.png)

### The minimum disk size is **16GB**. 

![Connection Example](5.png)

### Add the CentOS iso image into the VM which just is created.
* Storage > Controller:IDE > Optical Drive

![Connection Example](6.png)

### Slightly increase Video Memory 32MB

![Connection Example](7.png)

### Start the VM 

![Connection Example](8.png)

# CentOS 7.1 1503 Installation Procedure

### Select **Install CentOS 7**

![Connection Example](9.png)

### Follow the instruction, and choose what you prefer

![Connection Example](10.png)
![Connection Example](11.png)

### Check two important things

* Please check following two things :
1) Installation source should be **Local media** (mandatory)
2) Software selection is recommended as **GNOME DESKTOP** (option)

![Connection Example](12.png)

### Set USER Settings

* Give an user as **Administrator** permission

![Connection Example](13.png)
![Connection Example](14.png)
![Connection Example](15.png)

### Reboot the machine

![Connection Example](16.png)


# ESS Development Environment Setup

* Version : the script will ask an user which ESS DM version one would like to install
* Selected Option (TRUE) : EEE Local installation / CS-Studio 
* Non-selected Option (FALSE) : ESS Active Directory service / OpenXAL / IPython


### Login

![Connection Example](17.png)

### Open an terminal

![Connection Example](18.png)

* Set the font size as 9 for better view (optional)

![Connection Example](19.png)

* Set the scroll-bar as unlimited for easy tracking on the installation log (optional)

![Connection Example](20.png)


### Download the script

```
$ curl -L https://git.io/vi8DA -o dm_setup.bash
```

![Connection Example](21.png)

### Run the script

```
$ bash dm_setup.bash
```

### Select the release version 

* Please see the following site for more information.

https://ess-ics.atlassian.net/wiki/x/MoL-Ag

![Connection Example](23.png)

* With "gui" option, one can see the rsync-epics.log in another window. It is just another convenient terminal to show rsync status, so one can monitor it by oneself. 

![Connection Example](24.png)

* After the service is ready, one actually see the progress one by one.

![Connection Example](25.png)
![Connection Example](26.png)

* the procedure of the script is done, however, RSYNC is still in progress if network is slow. EPICS environment is not ready, but CS-Studio is ready. 

![Connection Example](27.png)
![Connection Example](28.png)
![Connection Example](29.png)
![Connection Example](30.png)
![Connection Example](31.png)
![Connection Example](32.png)


* Rsync is done, yes, that's it. Technically, you are  ready to use.

![Connection Example](33.png)


### Run the SoftIOC via iocsh
* Open another terminal
* type iocsh
```
iocsh
```

![Connection Example](34.png)
![Connection Example](35.png)

### Run CS-Studio 
```
css
```

![Connection Example](36.png)

![Connection Example](37.png)


## Random Failure
* Rsync procedure and CSS download might be failed from time to time, when the network connection speed is too slow. I saw some case, there is no css binary file in local disk after ansible did its job correctly. 

