<div class="btn-group" style="margin: 9px 0 25px;">
  <p style="margin-bottom:0;">
    <a href="<?php echo base_url();?>index.php/doctor/" class="btn  btn-primary" style="margin-left:0;">返回列表</a>
    
  </p>
</div>
<form id="rec_form" class="form-horizontal" action="<?php echo base_url();?>index.php/doctor/update" method="post">
   <input type="hidden" id="id" name="id" value="<?php
      echo $id;
     ?>" />
   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">中文姓名</label>
    <div class="controls">
     <input id="cn_name" name="cn_name" type="title" class="input" value="<?php
      echo $cn_name;
     ?>" required="" onkeyup="" autocomplete="off" placeholder="不超过12个字"/>
    </div>
   </div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">英文姓名</label>
    <div class="controls">
     <input id="en_name" name="en_name" type="title" class="input" value="<?php
      echo $en_name;
     ?>" required="" onkeyup="" autocomplete="off" placeholder="不超过12个字"/>
    </div>
   </div>


<div class="control-group" id="">
  <label class="control-label" for="thumb">医生照片(360x450)</label>
  <div class="controls">
    <input type="file" id="file" style="visibility:hidden;position:absolute;top:-9999em" onchange="transfer(this.files, $('#dropBox_1')[0])" />
    <input name="thumb" type="hidden" id="pic1" size="50" readonly="readonly" value="<?php
      echo $thumb;
     ?>"/>
  <div class="drop_img">
    <div class="uploadpic" id="dropBox_1" data-width="0" data-height="0" nth="1">
    <label for="file" style="width:100%;height:100%;"><img style="width:100%;height:100%;" id="pic_url" src="
<?php
      echo base_url('attached/image/'.$thumb);
     ?>
    "></label>
  </div>
  <div class="process" style="display:">
    <progress id="p_1" max="100" value="0"></progress>
    <span style="display:none"></span>
  </div>
    <span></span>
    </div>
  </div>
</div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">注册专科</label>
    <div class="controls">
     <select name="department" id="department">
       <?php 
        foreach ($departments as $key => $value) {
          ?>
            <option value="<?php echo $value['name']?>" <?php 
              if($department === $value['name']) {
                echo 'selected';
              }
            ?>
            ><?php echo $value['name']?></option>
          <?php
        }
       ?>
     </select>
    </div>
   </div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">电话号码</label>
    <div class="controls">
     <input id="tel" name="tel" type="title" class="number" value="<?php
      echo $tel;
     ?>" required="" onkeyup="" autocomplete="off" placeholder="不超过12个字"/>
    </div>
   </div>


   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">联系地址</label>
    <div class="controls">
     <input id="addr" name="addr" type="title" class="number" value="<?php
      echo $addr;
     ?>" required="" onkeyup="" autocomplete="off" placeholder="不超过100个字"/>
    </div>
   </div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">收费标准</label>
    <div class="controls">
     <input id="charge" name="charge" type="title" class="number" value="<?php
      echo $charge;
     ?>" required="" onkeyup="" autocomplete="off" placeholder="支持输入整数数字"/>$起
    </div>
   </div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">所在城市</label>
    <div class="controls">
     <select name="city" id="city">
       <?php 
        foreach ($citys as $key => $value) {
          ?>
            <option value="<?php echo $value['name']?>" <?php 
              if($city === $value['name']) {
                echo 'selected';
              }
            ?>
            ><?php echo $value['name']?></option>
          <?php
        }
       ?>
     </select>
    </div>
   </div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">专业资格</label>
    <div class="controls">
     <input id="skill" name="skill" type="title" class="text" value="<?php
      echo $skill;
     ?>" required="" onkeyup="" autocomplete="off" placeholder="请用逗号或分号分隔专业资格"/>
   <div>
    <br />
      <p>填写标准范例：</p>
      <p>香港理工大学物理疗学士1995，澳洲麦格里大学硕士2009</p>
    </div>
      </div>
    
   </div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">诊治时间</label>
    <div class="controls">
     <input id="time" name="time" type="title" class="text" value="<?php
      echo $time;
     ?>" required="" onkeyup="" autocomplete="off" placeholder="请用逗号或分号分隔诊治时间"/>
    
    <div>
    <br />
      <p>填写标准范例：</p>
      <p>星期一：9：30-10：00 10：30-11：00， 星期五：16：00</p>
    </div>
    </div>

   </div>


   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">提供医疗程序及手术</label>
    <div class="controls">
     <input id="operate" name="operate" type="title" class="text" value="<?php
      echo $operate;
     ?>" required="" onkeyup="" autocomplete="off" placeholder=""/>
    </div>
   </div>

   <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">使用医院或其他服务诊所</label>
    <div class="controls">
     <input id="hospital" name="hospital" type="title" class="text" value="<?php
      echo $hospital;
     ?>" required="" onkeyup="" autocomplete="off" placeholder=""/>
    </div>
   </div>

    <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">诊所以外提供的服务</label>
    <div class="controls">
     <input id="service" name="service" type="title" class="text" value="<?php
      echo $service;
     ?>" required="" onkeyup="" autocomplete="off" placeholder=""/>
    </div>
   </div>

       <div class="control-group" id="aboutSearchBox">
    <label class="control-label" for="name">其他资料</label>
    <div class="controls">
     <textarea id="other" name="other"><?php
      echo $other;
     ?></textarea>  
    </div>
   </div>

   <div class="control-group">
    <div class="controls">
     <button type="submit" class="btn btn-primary" id="submit">提交</button>
     <button type="button" class="btn" onclick="common.hideFancyBox()" style="margin-left:14px;">取消</button>
    </div>
   </div>
</form>
<script charset="utf-8" src="<?php echo base_url();?>/js/upload.js"></script>