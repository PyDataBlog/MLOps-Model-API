---
layout: post
title:  "Liunx服务器运维杂记"
date:   2017-11-18 20:25:27 +0800
categories: 学习笔记
tag: 运维杂记
---

* content
{:toc}



#### 1.Xshell远程连接服务器失败（Connection closed by foreign host.） 解决方案
##### 
#### 2.没有安装 vim 报错： -bash: vim: command not found
1.输入命令
``` 
rpm -qa|grep vim 
```
通常情况应出现下面三条
``` 
vim-enhanced-7.0.109-7.el5
vim-minimal-7.0.109-7.el5
vim-common-7.0.109-7.el5
```
若不足三个：则安装对应的 命令如下：
``` 
yum -y install vim-enhanced
```
若三个都没有则
``` 
yum -y install vim*
```

#### 3.环境部署
```
 tar -zxv -f 
```

#### 4.安装Redis-> make test 报错
```
You need tcl 8.5 or newer in order to run the Redis test
```


[jekyll]:      http://jekyllrb.com
[jekyll-gh]:   https://github.com/jekyll/jekyll
[jekyll-help]: https://github.com/jekyll/jekyll-help
