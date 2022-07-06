--系统管理
insert into SYSTEM_MENU(ID, MENUNAME, MENUURL, PARENTID) values(1, '菜单管理', '',0)
insert into SYSTEM_MENU(ID, MENUNAME, MENUURL, PARENTID) values(2, '用户管理', '',0)
insert into SYSTEM_MENU(ID, MENUNAME, MENUURL, PARENTID) values(3, '角色管理', '',0)
insert into SYSTEM_MENU(ID, MENUNAME, MENUURL, PARENTID) values(4, '权限管理', '',0)
--角色管理
insert into SYSTEM_ROLE(ID, ROLENAME) values(1, '超级管理员',)
insert into SYSTEM_ROLE(ID, ROLENAME) values(2, '普通管理员',)
insert into SYSTEM_ROLE(ID, ROLENAME) values(3, '普通用户',)
--用户管理
insert into SYSTEM_USER(USERID, ROLENAME) values(1, '超级管理员',)
