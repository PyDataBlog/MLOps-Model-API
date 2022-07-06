<?php /* Smarty version Smarty-3.1.10, created on 2014-07-08 17:30:38
         compiled from "D:\phpfind\WWW\themes\default\header.html" */ ?>
<?php /*%%SmartyHeaderCode:143353bbba3e25eb53-92782547%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '36a2c2cd7233b83ae477006fde1bfc60f4006267' => 
    array (
      0 => 'D:\\phpfind\\WWW\\themes\\default\\header.html',
      1 => 1404555682,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '143353bbba3e25eb53-92782547',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'site_url' => 0,
    'site_title' => 0,
    'cate' => 0,
    'cfg' => 0,
    'site_root' => 0,
    'site_path' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1.10',
  'unifunc' => 'content_53bbba3e5be0a1_32061055',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_53bbba3e5be0a1_32061055')) {function content_53bbba3e5be0a1_32061055($_smarty_tpl) {?>	<div id="header">
    	<div id="topbox">
    		<a href="<?php echo $_smarty_tpl->tpl_vars['site_url']->value;?>
" class="logo" title="<?php echo $_smarty_tpl->tpl_vars['site_title']->value;?>
"></a>
            <div id="sobox">
        	<form name="sofrm" class="sofrm" method="get" action="" onSubmit="return rewrite_search()">
            	<input name="mod" type="hidden" id="mod" value="search" />
                <input name="type" type="hidden" id="type" value="name" />
                <div id="selopt">
                	<div id="cursel">网站名称</div>
                    <ul id="options">
                    	<li><a href="javascript: void(0);" name="name">网站名称</a></li>
                    </ul>
                </div>
				<input name="query" type="text" class="sipt" id="query" onFocus="this.value='';" /><input type="submit" class="sbtn" value="搜 索" />
			</form>
			</div>
        </div>
        <div id="navbox">
			<ul class="navbar">
        		<li><a href="?mod=index">首页</a></li><li class="navline"></li>
				<?php  $_smarty_tpl->tpl_vars['cate'] = new Smarty_Variable; $_smarty_tpl->tpl_vars['cate']->_loop = false;
 $_from = get_categories(); if (!is_array($_from) && !is_object($_from)) { settype($_from, 'array');}
foreach ($_from as $_smarty_tpl->tpl_vars['cate']->key => $_smarty_tpl->tpl_vars['cate']->value){
$_smarty_tpl->tpl_vars['cate']->_loop = true;
?>
				<li><a href="<?php echo $_smarty_tpl->tpl_vars['cate']->value['cate_link'];?>
" title="<?php echo $_smarty_tpl->tpl_vars['cate']->value['cate_name'];?>
"><?php echo $_smarty_tpl->tpl_vars['cate']->value['cate_name'];?>
</a></li><li class="navline"></li>
				<?php } ?>
				<li><a href="http://bbs.lxsn.net/" rel="nofollow">站长论坛</a></li><li class="navline"></li>				
        	</ul>
        </div>
        <div id="txtbox">
        	<div class="count"><?php echo $_smarty_tpl->tpl_vars['cfg']->value['site_notice'];?>
</div>
<div class="link">快捷方式：<a href="<?php echo $_smarty_tpl->tpl_vars['site_root']->value;?>
member/?mod=website&act=add">站点提交</a> - <a href="<?php echo $_smarty_tpl->tpl_vars['site_root']->value;?>
member/?mod=article&act=add">软文投稿</a></div>
        </div>
    </div>
    <div class="sitepath"><?php echo $_smarty_tpl->tpl_vars['site_path']->value;?>
</div><?php }} ?>