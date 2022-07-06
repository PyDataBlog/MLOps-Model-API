#chmod

change a file's access permision:

```
chomd mode file
```

```
chmod  -R 777 [dir_name]    //所有人可以读，写，执行
chmod 777 [file_name]		  
chmod a=rwx [file_name]     
chmod ugo+r [file_name]    //所有人可以读
chmod a+r  [file_name]     
chmod ug+w,o-w [filename]  //用户和用户组内用户可以写，其他人可以写 。
chmod -R a+r *
```

first 7 -->User

second 7-->Group

third 7 -->Other
				 all

r=4

w=2

x=1

rwx=4+2+1=7

rw-=4+2=6

r-x=4+1=5

提交文件的时候用：

```
chmod  -R 777 [dir_name]    //所有人可以读，写，执行
```
