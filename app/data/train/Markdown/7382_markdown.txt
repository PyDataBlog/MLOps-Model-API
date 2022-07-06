#Git教程

###写在前面

一点小历史，不喜欢的请[点我跳过](#start)

> 到了2002年，Linux系统已经发展了十年了，代码库之大让Linus很难继续通过手工方式管理了，社区的弟兄们也对这种方式表达了强烈不满，于是Linus选择了一个商业的版本控制系统BitKeeper，BitKeeper的东家BitMover公司出于人道主义精神，授权Linux社区免费使用这个版本控制系统。

> 安定团结的大好局面在2005年就被打破了，原因是Linux社区牛人聚集，不免沾染了一些梁山好汉的江湖习气。开发Samba的Andrew试图破解BitKeeper的协议（这么干的其实也不只他一个），被BitMover公司发现了（监控工作做得不错！），于是BitMover公司怒了，要收回Linux社区的免费使用权。

> Linus可以向BitMover公司道个歉，保证以后严格管教弟兄们，嗯，这是不可能的。实际情况是这样的：

> Linus花了两周时间自己用C写了一个分布式版本控制系统，*这就是Git*！一个月之内，Linux系统的源码已经由Git管理了！牛是怎么定义的呢？大家可以体会一下。

> Git迅速成为最流行的分布式版本控制系统，尤其是2008年，GitHub网站上线了，它为开源项目免费提供Git存储，无数开源项目开始迁移至GitHub，包括jQuery，PHP，Ruby等等。

##<a name="start"></a>零.额外的问题

###1.vim显示中文乱码

命令行直接用vim编辑会出现汉字乱码，而且用QQ拼音输入法在vim里面wu法输入某些汉字，比如中wen的wen、还有wu法的wu会变成1
utf8乱码问题还没有解决（找到的解决方案皆无法破之），建议不要用vim直接编辑

###2.MINGW32默认不支持复制粘贴，需要手动设置

1.  右击标题栏，选择【属性】

2.  选择【选项】

3.  勾选【快速编辑模式Q】

4.  【确定】-【确定】

设置立即生效，支持复制/粘贴：

-  复制：左键拖选，然后右击即完成复制

-  粘贴：在窗口内右击即可

如有疑问请查看[ChinaUnix博客：windows下mingw的复制粘贴](http://blog.chinaunix.net/uid-24709751-id-4032541.html)

##一.安装git

在Windows/Mac/Linux上安装git的方法请查看[廖雪峰的官方网站：安装Git](http://www.liaoxuefeng.com/wiki/0013739516305929606dd18361248578c67b8067c8c017b000/00137396287703354d8c6c01c904c7d9ff056ae23da865a000)

P.S.廖雪峰前辈的git教程非常不错，但你肯定没耐心从头看到尾，不过没关系，因为我看过了，而且会尽量没有废话地总结出来

##二.本地操作

###1.创建版本库

1.  先找一个顺眼的文件夹，`cd`过去，准备在里面创建版本库（Repository）

  P.S.因为git命令行工具其实就是一个简版Linux虚拟机，支持Shell命令，如果记得一些Linux命令的话会用得比较顺手，常用Shell命令请查看[博客园：常用的文件和目录操作命令(转)](http://www.cnblogs.com/ggjucheng/archive/2012/08/22/2650464.html)

2.  初始化一个Git仓库，使用`git init`命令

  可以把当前目录初始化为git仓库，其实也就是在目录下自动生成一些git管理信息

3.  新建文件

  在该目录下新建文件，命令行`touch`或者资源管理器右键创建都可以

4.  添加文件到Git仓库，分两步：

    1.  使用命令`git add <file>`把文件放进暂存区，注意，可反复多次使用，添加多个文件；

    2.  使用命令`git commit`提交暂存区的修改，完成。
    
  P.S.在文件夹下创建的文件以及对文件内容的增删改不是像同步网盘一样自动同步的，需要add-commit才能创建一个新版本

###2.日常工作

1.  查看状态

  要随时掌握工作区的状态，使用`git status`命令。

  如果`git status`告诉你有文件被修改过，用`git diff`可以查看修改内容。

2.  版本后退与前进

  HEAD指向的版本就是当前版本，因此，Git允许我们在版本的历史之间穿梭，使用命令`git reset --hard commit_id`。

  穿梭前，用`git log`可以查看提交历史，以便确定要回退到哪个版本。

  要重返未来，用`git reflog`查看命令历史，以便确定要回到未来的哪个版本。

3.  撤销修改

    -  场景1：当你改乱了工作区某个文件的内容，想直接丢弃工作区的修改时，用命令`git checkout -- file`。

    -  场景2：当你不但改乱了工作区某个文件的内容，还添加到了暂存区时，想丢弃修改，分两步，第一步用命令`git reset HEAD file`，就回到了场景1，第二步按场景1操作。

    -  场景3：已经提交了不合适的修改到版本库时，想要撤销本次提交，参考版本回退一节，不过前提是没有推送到远程库。

4.  删除文件

  命令`git rm`用于删除一个文件。如果一个文件已经被提交到版本库，那么你永远不用担心误删，但是要小心，你只能恢复文件到最新版本，你会丢失最近一次提交后你修改的内容。
  
5.  操作分支

  在实际开发中，我们应该按照几个基本原则进行分支管理：

  首先，`master`分支应该是非常稳定的，也就是仅用来发布新版本，平时不能在上面干活；

  那在哪干活呢？干活都在`dev`分支上，也就是说，`dev`分支是不稳定的，到某个时候，比如1.0版本发布时，再把`dev`分支合并到`master`上，在`master`分支发布1.0版本；

  你和你的小伙伴们每个人都在`dev`分支上干活，每个人都有自己的分支，时不时地往`dev`分支上合并就可以了。

  合并分支时，加上`--no-ff`参数就可以用普通模式合并，合并后的历史有分支，能看出来曾经做过合并，而`fast forward`合并就看不出来曾经做过合并。例如：`git merge --no-ff -m "merge with no-ff" dev`

  具体命令：
  
    -  查看分支：`git branch`

    -  创建分支：`git branch <name>`

    -  切换分支：`git checkout <name>`

    -  创建+切换分支：`git checkout -b <name>`

    -  合并某分支到当前分支：`git merge <name>`

    -  删除分支：`git branch -d <name>`
    
  当Git无法自动合并分支时，就必须首先解决冲突。解决冲突后，再提交，合并完成。

  Git用`<<<<<<<`，`=======`，`>>>>>>>`标记出不同分支的内容，手动修改消除冲突后再`add-commit`就好了

  用`git log --graph`命令可以看到分支合并图
  
6.  处理bug

  修复bug时，我们会通过创建新的bug分支进行修复，然后合并，最后删除；

  当手头工作没有完成时，先把工作现场`git stash`一下，然后去修复bug，修复后，再`git stash pop`，回到工作现场。

7.  开发新功能

  开发一个新feature，最好新建一个分支；

  如果要丢弃一个没有被合并过的分支，可以通过`git branch -D <name>`强行删除。
  
###3.人性化配置选项

1.  忽略特殊文件

  忽略某些文件时，需要编写.gitignore；

  .gitignore文件本身要放到版本库里，并且可以对.gitignore做版本管理

2.  设置命令别名

  普遍接受的别名（类似于宏一样的东西，用来保护手指）：

    -  st就表示status，命令`git config --global alias.st status`

    -  用co表示checkout，命令`git config --global alias.co checkout`

    -  ci表示commit，命令`git config --global alias.ci commit`

    -  br表示branch，命令`git config --global alias.br branch`

3.  删除别名

  每个仓库的Git配置文件都放在`.git/config`文件中，别名就在[alias]后面，要删除别名，直接把对应的行删掉即可

  全局Git配置文件放在`~/gitconfig`文件中，可以直接编辑
  
4.  其它配置选项

  可以自定义的部分比较多，比如命令输出结果颜色方案（文件名高亮什么的）

###4.搭建git服务器

详细步骤请查看[廖雪峰的官方网站：搭建Git服务器](http://www.liaoxuefeng.com/wiki/0013739516305929606dd18361248578c67b8067c8c017b000/00137583770360579bc4b458f044ce7afed3df579123eca000)

##三.远程操作

远程操作是指本地项目与github上的项目时不时地同步一下

###0.准备工作

需要注册github帐号，创建版本库，设置公钥等等，具体请查看[廖雪峰的官方网站：添加远程库](http://www.liaoxuefeng.com/wiki/0013739516305929606dd18361248578c67b8067c8c017b000/0013752340242354807e192f02a44359908df8a5643103a000)

###1.关联远程库

要关联一个远程库，使用命令`git remote add origin git@server-name:path/repo-name.git`；

关联后，使用命令`git push -u origin master`第一次推送master分支的所有内容；

此后，每次本地提交后，只要有必要，就可以使用命令`git push origin master`推送最新修改；

分布式版本系统的最大好处之一是在本地工作完全不需要考虑远程库的存在，也就是有没有联网都可以正常工作，而SVN在没有联网的时候是拒绝干活的！当有网络的时候，再把本地提交推送一下就完成了同步，真是太方便了！

###2.克隆远程库

要克隆一个仓库，首先必须知道仓库的地址，然后使用`git clone`命令克隆。

Git支持多种协议，包括https，但通过ssh支持的原生git协议速度最快。

###3.多人协作

查看远程库信息，使用`git remote -v`；

本地新建的分支如果不推送到远程，对其他人就是不可见的；

从本地推送分支，使用`git push origin branch-name`，如果推送失败，先用`git pull`抓取远程的新提交；

在本地创建和远程分支对应的分支，使用`git checkout -b branch-name origin/branch-name`，本地和远程分支的名称最好一致；

建立本地分支和远程分支的关联，使用`git branch --set-upstream branch-name origin/branch-name`；

从远程抓取分支，使用`git pull`，如果有冲突，要先处理冲突。

###4.使用标签

标签是版本库的一个快照，常用在新版本发布前标记当前版本

1.  创建标签

  命令`git tag <name>`用于新建一个标签，默认为HEAD，也可以指定一个`commit id`；

  `git tag -a <tagname> -m "blablabla..."`可以指定标签信息；

  `git tag -s <tagname> -m "blablabla..."`可以用PGP签名标签；

  命令`git tag`可以查看所有标签。

2.  操作标签

  命令`git push origin <tagname>`可以推送一个本地标签；

  命令`git push origin --tags`可以推送全部未推送过的本地标签；

  命令`git tag -d <tagname>`可以删除一个本地标签；

  命令`git push origin :refs/tags/<tagname>`可以删除一个远程标签。

###5.fork sb on github

P.S.fork应该取自Linux的Shell命令fork（复制当前进程，得到的子进程与父进程互不影响），用来把别人的项目版本库转存到自己的github，之后就相互独立了

  在GitHub上，可以任意Fork开源仓库；

  自己拥有Fork后的仓库的读写权限；

  可以推送pull request给官方仓库来贡献代码。


###6.常用远程操作

1.  获取远程项目`git clone git@github.com:ayqy/git-helloworld.git`，会在当前目录下创建项目文件夹

2.  在线修改，通过`git pull origin master`拿到本地来

3.  本地修改，通过`git push -u origin master`上传同步（要注意先add-commit）

##四.常用命令

-  `mkdir：XX` (创建一个空目录 XX指目录名)

-  `pwd`：显示当前目录的路径。

-  `cat XX`：查看XX文件内容

-  `git init`：把当前的目录变成可以管理的git仓库，生成隐藏.git文件。

-  `git add XX`：把xx文件添加到暂存区去。

-  `git commit –m “XX”`：提交文件 –m 后面的是注释。

-  `git status`：查看仓库状态

-  `git diff  XX`：查看XX文件修改了那些内容

-  `git log`：查看历史记录

-  `git reset`  --hard HEAD^ 或者 git reset  --hard HEAD~：回退到上一个版本

  P.S.如果想回退到100个版本，使用git reset –hard HEAD~100
  
-  `git reflog`：查看历史记录的版本号id

-  `git checkout -- XX`：把XX文件在工作区的修改全部撤销。

-  `git rm XX`：删除XX文件

-  `git remote add origin https://github.com/ayqy/test.git`：关联一个远程库

-  `git push –u(第一次要用-u 以后不需要) origin master`：把当前master分支推送到远程库

-  `git clone https://github.com/ayqy/test.git`：从远程库中克隆

-  `git checkout –b dev`：创建dev分支 并切换到dev分支上

-  `git branch`：查看当前所有的分支

-  `git checkout master`：切换回master分支

-  `git merge dev`：在当前的分支上合并dev分支

-  `git branch –d dev`：删除dev分支

-  `git branch name`：创建分支

-  `git stash`：把当前的工作隐藏起来 等以后恢复现场后继续工作

-  `git stash list`：查看所有被隐藏的文件列表

-  `git stash apply`：恢复被隐藏的文件，但是内容不删除

-  `git stash drop`：删除文件

-  `git stash pop`：恢复文件的同时 也删除文件

-  `git remote`：查看远程库的信息

-  `git remote –v`：查看远程库的详细信息

-  `git push origin master`：Git会把master分支推送到远程库对应的远程分支上

-  `git push origin :branch-name`：冒号前面的空格不能少，原理是把一个空分支push到server上，相当于删除该分支

##五.命令大全

###1.CREATE

- Clone an existing repository
  
  `git clone ssh://user@domain.com/repo.git`

- Create a new local repository

  `git init`

###2.LOCAL CHANGES

- Changed files in your working directory

  `git status`

- Changes to tracked files

  `git diff`

- Add all current changes to the next commit

  `git add .`

- Add some changes in <file> to the next commit

  `git add -p <file>`

- Commit all local changes in tracked files

  `git commit -a`

- Commit previously staged changes

  `git commit`

- Change the last commit *Don‘t amend published commits!*

  `git commit --amend`

###3.COMMIT HISTORY

- Show all commits, starting with newest
  
  `git log`

- Show changes over time for a specific file

  `git log -p <file>`

- Who changed what and when in <file>

  `git blame <file>`

###4.BRANCHES & TAGS

- List all existing branches

  `git branch -av`

- Switch HEAD branch

  `git checkout <branch>`

- Create a new branch based on your current HEAD

  `git branch <new-branch>`

- Create a new tracking branch based on a remote branch

  `git checkout --track <remote/branch>`

- Delete a local branch

  `git branch -d <branch>`

- Mark the current commit with a tag

  `git tag <tag-name>`

###5.UPDATE & PUBLISH

- List all currently configured remotes

  `git remote -v`

- Show information about a remote

  `git remote show <remote>`

- Add new remote repository, named <remote>

  `git remote add <shortname> <url>`

- Download all changes from <remote>, but don‘t integrate into HEAD

  `git fetch <remote>`

- Download changes and directly merge/integrate into HEAD

  `git pull <remote> <branch>`

- Publish local changes on a remote

  `git push <remote> <branch>`

- Delete a branch on the remote

  `git branch -dr <remote/branch>`

- Publish your tag s

  `git push --tags`

###6.MERGE & REBASE

- Merge <branch> into your current HEAD

  `git merge <branch>`

- Rebase your current HEAD onto <branch> *Don‘t rebase published commits!*

  `git rebase <branch>`

- Abort a rebase

  `git rebase --abort`

- Continue a rebase after resolving conflicts

  `git rebase --continue`

- Use your configured merge tool to solve conflicts

  `git mergetool`

- Use your editor to manually solve conflicts and (after resolving) mark file as resolved

  `git add <resolved-file>`

  `git rm <resolved-file>`

###7.UNDO

- Discard all local changes in your working directory

  `git reset --hard HEAD`

- Discard local changes in a specific file

  `git checkout HEAD <file>`

- Revert a commit (by producing a new commit with contrary changes)

  `git revert <commit>`

- Reset your HEAD pointer to a previous commit

  …and discard all changes since then：`git reset --hard <commit>`

  …and preserve all changes as unstaged changes：`git reset <commit>`

  …and preserve uncommitted local changes：`git reset --keep <commit>`

###参考资料

-  [廖雪峰的官方网站：Git教程](http://www.liaoxuefeng.com/wiki/0013739516305929606dd18361248578c67b8067c8c017b000)

-  [博客园：Git使用教程](http://www.cnblogs.com/tugenhua0707/p/4050072.html)