--删除
drop table t_users;
drop table t_admin;
drop table t_type;
drop table t_mintype;
drop table t_smin;
drop table t_commodity;
drop table t_num;
drop table t_needcom;
drop table comments;
drop table collects;
drop table dingdan;
--查询
select * from t_users;
select * from t_admin;
select * from t_type;
select * from t_mintype;
select * from t_smin;
select * from t_commodity;
select * from t_num;
select * from t_needcom;
select * from comments;
select * from collects;
select * from dingdan;


--用户信息表(买家卖家注册时所需填写的信息, 用于用户核实)
create table t_users(
	userid Integer primary key,							--用户编号
	username varchar2(20) not null unique,				--用户名		(不能为空)
	upassword varchar2(40) not null,					--用户密码		(不能为空)
	ulogo varchar2(100) ,								--用户头像
	usex varchar(2) ,									--性别
	uphone varchar2(20) not null,						--用户联系电话	(不能为空--为短信注册)
	uqq varchar2(20),									--用户QQ
	uemail varchar2(20) ,								--用户邮箱
	uaddress varchar2(200),								--用户地址
  udate date                            --用户注册时间
);

create sequence seq_t_users increment by 1 start with 1;
insert into t_users values(seq_t_users.nextval, '张三', '123456', '', '男', '15386003052', '1451932421', '1451932421@qq.com', '海南',sysdate);
insert into t_users values(seq_t_users.nextval, '李四', '123456', '', '女', '18423131532', '1231351315', '1231351315@qq.com', '北京',sysdate);
insert into t_users values(seq_t_users.nextval, '王五', '123456', '', '女', '18423131532', '1231351315', '1231351315@qq.com', '上海',sysdate);

--管理员信息表(管理员操作)
create table t_admin(
	adminid Integer primary key,						--管理员编号
	adminname varchar(20) not null unique,	--管理员用户名		(不能为空)
	apassword varchar2(40) not null,				--管理员密码		(不能为空)
  aname varchar2(20) not null,            --管理员真实姓名
  asex varchar2(2)  not null,             --管理员性别
	aphone varchar2(20) not null,						--管理员联系电话	(不能为空)
	aemail varchar2(20) not null,						--管理员邮箱		(不能为空)
	alevel Integer default 2							  --管理员权限等级 (1.超级管理员   2.普通管理员)
);

select * from T_COMMODITY where cid = 48;

select * from comments where comcid = 48;

select cusername,cdate,cprice,cpic,caddress from T_COMMODITY where cid = 49;

create sequence seq_t_admin increment by 1 start with 1;
insert into t_admin values(seq_t_users.nextval,'admin','a','admin','男','15386003052','1451932421@qq.com',1);
insert into t_admin values(seq_t_users.nextval,'LSY','a','LSY','男','15386003052','1451932421@qq.com',2);
insert into t_admin values(seq_t_users.nextval,'ZMY','a','ZMY','女','15386003052','1451932421@qq.com',2);
insert into t_admin values(seq_t_users.nextval,'ZYQ','a','ZYQ','男','15386003052','1451932421@qq.com',2);

--商品总类型表(记录商品的种类, 如:电子产品)
create table t_type(
	tid Integer primary key,							--总类型编号
	tname varchar2(10) not null unique					--类型名称	(如: 电子产品, 学习用品..===>> 发布商品时选择的类型.)
);

insert into t_type values(1, '闲置数码');
insert into t_type values(2, '学习用品');
insert into t_type values(3, '影音家电');
insert into t_type values(4, '鞋服配饰');
insert into t_type values(5, '珠宝收藏');
insert into t_type values(6, '家具日用');
insert into t_type values(7, '其他');

update t_type set tname = '家具日用' where tid = 6;


--商品小类型表(细化总类型, 电子产品里的:手机, 电脑....)
create table t_mintype(
  mid Integer primary key,                --小类型id
	tid references t_type(tid),							--类型编号( 外键引用商品总类型 )
	mname varchar2(50) not null unique					--类型名称(如: 手机, 电脑, 耳机 .==>> 模糊查询)
);

create sequence seq_t_mintype increment by 1 start with 24;
insert into t_mintype values(1,1,'手机');
insert into t_mintype values(2,1,'电脑/电脑周边');
insert into t_mintype values(3,1,'电玩随身听');
insert into t_mintype values(4,1,'相机/单反');
insert into t_mintype values(5,2,'书籍');
insert into t_mintype values(6,2,'四六级/考研');
insert into t_mintype values(7,2,'学习用品');
insert into t_mintype values(8,3,'大家电');
insert into t_mintype values(9,3,'生活小家电');
insert into t_mintype values(10,3,'厨房电器');
insert into t_mintype values(11,3,'影音/耳机/HIFI');
insert into t_mintype values(12,4,'女装');
insert into t_mintype values(13,4,'女鞋');
insert into t_mintype values(14,4,'箱包');
insert into t_mintype values(15,4,'配饰');
insert into t_mintype values(16,5,'珠宝');
insert into t_mintype values(17,5,'手表');
insert into t_mintype values(18,5,'饰品');
insert into t_mintype values(19,5,'收藏品');
insert into t_mintype values(20,6,'家具');
insert into t_mintype values(21,6,'家纺家饰');
insert into t_mintype values(22,6,'日用日化');
insert into t_mintype values(23,6,'家装建材');

select * from t_mintype;

--第三级小分类表
create table t_smin(
  smid Integer primary key,                --小类型id
	mid references t_mintype(mid),							--类型编号( 外键引用商品小类型类型 )
	smname varchar2(50) not null unique					--类型名称(如: 手机 ==>> 手机壳 , 数据线)
);

create sequence seq_t_smin increment by 1 start with 1;
insert into t_smin values(seq_t_smin.nextval,1,'手机壳');
insert into t_smin values(seq_t_smin.nextval,1,'数据线');
insert into t_smin values(seq_t_smin.nextval,2,'笔记本');
insert into t_smin values(seq_t_smin.nextval,2,'PC电脑');
insert into t_smin values(seq_t_smin.nextval,3,'PS4');
insert into t_smin values(seq_t_smin.nextval,3,'MP4');
insert into t_smin values(seq_t_smin.nextval,4,'单反');
insert into t_smin values(seq_t_smin.nextval,4,'镜头');
insert into t_smin values(seq_t_smin.nextval,5,'Java');
insert into t_smin values(seq_t_smin.nextval,5,'Oracle');
insert into t_smin values(seq_t_smin.nextval,6,'计算机安全');
insert into t_smin values(seq_t_smin.nextval,6,'数据挖掘');
insert into t_smin values(seq_t_smin.nextval,7,'笔记');
insert into t_smin values(seq_t_smin.nextval,8,'电视');
insert into t_smin values(seq_t_smin.nextval,8,'冰箱');
insert into t_smin values(seq_t_smin.nextval,9,'吹风机');
insert into t_smin values(seq_t_smin.nextval,9,'风扇');
insert into t_smin values(seq_t_smin.nextval,10,'豆浆机');
insert into t_smin values(seq_t_smin.nextval,10,'电饭煲');
insert into t_smin values(seq_t_smin.nextval,11,'耳机');
insert into t_smin values(seq_t_smin.nextval,11,'音响');
insert into t_smin values(seq_t_smin.nextval,12,'连衣裙');
insert into t_smin values(seq_t_smin.nextval,12,'T恤');
insert into t_smin values(seq_t_smin.nextval,13,'运动鞋');
insert into t_smin values(seq_t_smin.nextval,13,'单鞋');
insert into t_smin values(seq_t_smin.nextval,14,'女包');
insert into t_smin values(seq_t_smin.nextval,14,'旅行箱');
insert into t_smin values(seq_t_smin.nextval,15,'帽子');
insert into t_smin values(seq_t_smin.nextval,15,'围巾');
insert into t_smin values(seq_t_smin.nextval,16,'翡翠');
insert into t_smin values(seq_t_smin.nextval,16,'黄金');
insert into t_smin values(seq_t_smin.nextval,17,'电子表');
insert into t_smin values(seq_t_smin.nextval,17,'石英表');
insert into t_smin values(seq_t_smin.nextval,18,'眼睛');
insert into t_smin values(seq_t_smin.nextval,18,'手链');
insert into t_smin values(seq_t_smin.nextval,19,'古玩');
insert into t_smin values(seq_t_smin.nextval,19,'邮票');
insert into t_smin values(seq_t_smin.nextval,20,'茶几');
insert into t_smin values(seq_t_smin.nextval,20,'沙发');
insert into t_smin values(seq_t_smin.nextval,21,'地毯');
insert into t_smin values(seq_t_smin.nextval,21,'窗帘');
insert into t_smin values(seq_t_smin.nextval,22,'伞');
insert into t_smin values(seq_t_smin.nextval,22,'雨衣');
insert into t_smin values(seq_t_smin.nextval,23,'台灯');
insert into t_smin values(seq_t_smin.nextval,23,'壁灯');

select * from t_smin;

	select * from comments where comcid = 48;
 
select * from (select * from t_commodity order by  cdate DESC ) where rownum<5;

select cusername,cdate,cprice,cpic,caddress from T_COMMODITY where cid=49;

--商品发布表(发布商品时所需告知, 显示的信息表)
--drop table t_commodity;
create table t_commodity(
	cid Integer primary key,							--商品编号
	cname varchar2(30) not null,						--商品名称/标题		(不能为空)
	ctname varchar2(10) not null,						--商品类型			(不能为空)
	cdescribe varchar2(400) default '卖家有点懒, ',		--商品描述	
	cpic varchar2(100),						--商品图片信息(  可多张, 以 ; 分隔  )
	cprice float not null,								--商品出售价格		(不能为空)
	cusername varchar2(20) not null,					--商品卖家( 发布商品的用户 )
	caddress varchar2(40),								--用户当前所在地
	cphone varchar2(20)	not null,						--商家联系电话		(不能为空)
	cpostage Integer default 2,							--是否包邮 ( 默认不包邮 )
	cqq	varchar2(20),									--商家qq
	cdate date ,										--商品发布时间
	cstate_com Integer default 1,						--商品状态(1.未交易  2.已交易)
	cstate_dingdan Integer default 2,							--订单审核状态(1.待审核 2.通过 3.未通过)
	cmessage varchar2(100) default '无',							--审核信息(比如未通过: 理由)
	cadminname varchar2(20)								--订单审核的管理员
);
commit;
create sequence seq_t_commodity increment by 1 start with 1;
select * from T_COMMODITY;
insert into t_commodity values(seq_t_commodity.nextval,'闲置手机','手机','韩版，移动联通双4G，指纹能用，外观轻微刮痕，9成新以上，喜欢的留言或者私聊','TB1QmdTPFXXXXc.XXXX_SIY7VXX_728x728.jpg;TB1MKJVPFXXXXcmXXXXreZZ7VXX_728x728.jpg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'Sony/索尼 DSC-W310数码相机','相机','Sony/索尼 DSC-W310数码相机 1200万像素4倍光变 广角卡片机 现闲置转给有需要的朋友，附赠相机包、内存卡、读卡器、迷你三脚架，由于搬家充电器找不到了，本人一直用万能充充电请买家熟知，二手物品售出概不退货，请买家购买前充分了解，买卖讲究诚信，本人并非经营二手物品，只是个人闲置物品出售。
','TB1dWFnKFXXXXXAXXXXtUp08pXX_728x728.jpg;TB282YFqVXXXXXeXXXXXXXXXXXX_!!0-fleamarket.jpg_728x728.jpg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'hp扫描仪','电脑','9成新，用不着了，没毛病，占地方需要的拿去','TB13pCCLpXXXXb_XVXX2WCJ.VXX_728x728.jpg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'【1212品牌盛典】【苏宁易购】lenovo/联想天','电脑','感兴趣的话给我留言吧！','TB1WLyjNVXXXXclXXXXGYteFXXX_728x728.jpg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');

insert into t_commodity values(seq_t_commodity.nextval,'【苏宁易购】lenovo/联想天','电脑','感兴趣的话给我留言吧！','TB1WLyjNVXXXXclXXXXGYteFXXX_728x728.jpg;15341797ae04551591.jpg;banner3.jpg;banner1.jpg;banner2.jpg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');


insert into t_commodity values(seq_t_commodity.nextval,'闲置手机','闲置数码','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'台式电脑','闲置数码','无','images/pic.jsg',2350,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'PS4','闲置数码','无','images/pic.jsg',1600,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'吹风机','影音家电','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'电饭煲','影音家电','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'豆浆机','影音家电','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'新T恤','鞋服配饰','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'板鞋','鞋服配饰','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'新织围巾','鞋服配饰','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'石英手表','珠宝收藏','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'长城邮票','珠宝收藏','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');
insert into t_commodity values(seq_t_commodity.nextval,'古玩','珠宝收藏','无','images/pic.jsg',2000,'LSY','海南','15386003052',2,'1451932421',sysdate,1,2,'无','admin');

select * from (select * from t_commodity order by  cdate DESC ) where rownum <5;
--商品浏览量(每被点击查看一次, 点击量加一)
create table t_num(										
	cid references t_commodity(cid),					--外键引用商品详情表id		
	nnum int default 0									--商品点击量( 绑定ip地址, 不可重复累加 )
);

create sequence seq_t_num increment by 1 start with 1;


--商品求购表(用户发布自己的求购信息)
create table t_needcom(
	nid Integer primary key,							--求购表编号
	nname varchar2(30) not null,						--求购商品名称/标题
	ndescribe varchar2(400) default '买家有点懒',		--求购商品描述
	npic varchar2(100),									--求购商品图片信息
	nusername varchar2(20) not null,					--发布求购用户
	naddress varchar2(40),								--用户当前所在地
	ndate date ,										--发布求购信息时间
	norder	Integer default 2,							--订单审核状态(1.待审核 2.通过 3.未通过)
	nmessage varchar2(100) default '无',							--审核信息(比如未通过: 理由)
	nadname varchar2(20)						--订单审核的管理员
);

create sequence seq_t_needcom increment by 1 start with 1;

insert into t_needcom values(seq_t_needcom.nextval,'求购一个闲置手机','无','images/pic2.jpg','张三','湖南',sysdate,2,'无','admin');

--商品评价表
create table comments(
	comid Integer primary key,							
	comcid Integer not null,							--指定哪一个商品id( 哪一个商品的留言)
	comcontent varchar2(100),							--发表内容
	comusername varchar2(20) not null,					--发表用户
	comdate date
);

select * from comments;
create sequence seq_comments increment by 1 start with 1;

		select comid, comcid,comusername,comcontent,to_date(sysdate,'yyyy-MM-DD') from comments where comcid = 65

		select comcid,comusername,to_char(comdate,'yyyy-MM-DD') from comments where comcid = 65;

insert into comments values(seq_comments.nextval, 49,'这个可小刀吗','zmy', toString(sysdate,'yyyy-MM-DD'));
insert into comments values(seq_comments.nextval, 65,'这个可小刀吗','zmy', to_date('19960203','yyyy-MM-dd'));
insert into comments values(seq_comments.nextval, 46,'这个可小刀吗','zmy',sysdate);
insert into comments values(seq_comments.nextval, 45,'这个可小刀吗','zmy',sysdate);


--收藏夹
create table collects(
	collid Integer primary key,							--收藏id	
	collusername varchar2(20) not null ,				--收藏用户
	collcid	Integer not null ,							--收藏的商品id
	collstate Integer default 1							--是否收藏 ( 1.未收藏	2.已收藏 )
);

create sequence seq_collects increment by 1 start with 1;

--订单表
create table dingdan(
	did Integer primary key,							--订单编号
	dno varchar2(20) not null unique,					--订单流水编号
	dcid Integer not null,                   						--商品id
	dcname varchar2(30) not null,						--商品名称/标题(外键引用发布商品信息表标题)
	daddress varchar2(200)	not null,					--收货地址
	dusername varchar2(20) not null,					--支付用户		
	ddate date,											--订单时间
	dmoney float check(dmoney > 0),						--订单交易额
	dstate Integer default 1							--订单状态(1.未交易成功  2.交易成功) 
);

create sequence seq_dingdan increment by 1 start with 1;
