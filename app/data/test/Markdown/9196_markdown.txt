# Linux下配置IP

在Linux系统中配置IP地址的方法有4种：

| 方法 | 说明 |
| -- | -- |
| ifconfig命令 | 通过```ifconfig```命令可以**临时**配置IP地址，但是```ifconfig```命令主要用于**查看**网络状态。 |
| setup工具 | setup工具可以永久配置IP地址，但setup是Redhat系列的**专有命令**。在使用```setup```之后，还需要运行命令```service network restart```以重启网络服务。 |
| **网络配置文件** | 通过修改网络配置文件来配置IP地址是标准的配置方法，必须掌握。 |
| 图形界面 | 与在Windows下配置IP的方法类似，仅作了解 |

## Linux网络配置文件

通过修改网络配置文件来配置IP，是以上4种方法中最重要且最值得掌握的方式。
首先，应该清楚Linux的网络配置文件有哪些（见下表）。

| 文件路径 | 文件名 | 说明 |
| -- | -- |
| /etc/sysconfig/net-scripts/ifcfg-eth0 | 网卡信息文件 |
| /etc/sysconfig/network | 主机名文件 |
| /etc/resolv.conf | DNS配置文件 |


**补充：**
1. DHCP服务器是一种自动分配IP地址的服务器。如果需要自动获取IP，则在局域网内要有DHCP服务器。

