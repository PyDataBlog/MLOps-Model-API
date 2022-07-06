<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <title>AdminLTE 2 | Data Tables</title>
  <!-- Tell the browser to be responsive to screen width -->
  <meta content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no" name="viewport">
  <!-- Bootstrap 3.3.6 -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/bootstrap/css/bootstrap.min.css">
  <!-- Font Awesome -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/font-awesome/css/font-awesome.min.css">
  <!-- Ionicons -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/Ionicons/css/ionicons.min.css">
  <!-- DataTables -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/datatables/dataTables.bootstrap.css">
  <!-- daterange picker -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/daterangepicker/daterangepicker.css">
  <!-- bootstrap datepicker -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/datepicker/datepicker3.css">
  <!-- iCheck for checkboxes and radio inputs -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/iCheck/all.css">
  <!-- Bootstrap Color Picker -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/colorpicker/bootstrap-colorpicker.min.css">
  <!-- Bootstrap time Picker -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/timepicker/bootstrap-timepicker.min.css">
  <!-- Select2 -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/select2/select2.min.css">
  <!-- Theme style -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/dist/css/AdminLTE.min.css">
  <!-- notif alert delete -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/plugins/sweetalert/sweetalert.css">
  <!-- AdminLTE Skins. Choose a skin from the css/skins
       folder instead of downloading all of them to reduce the load. -->
  <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>assets/dist/css/skins/_all-skins.min.css">


  <!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
  <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
  <!--[if lt IE 9]>
  <script src="https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"></script>
  <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
  <![endif]-->
</head>
<body class="hold-transition skin-blue sidebar-mini">
<div class="wrapper">

  <header class="main-header">
    <!-- Logo -->
    <a href="<?php echo base_url(); ?>index.php/home/" class="logo">
      <!-- mini logo for sidebar mini 50x50 pixels -->
      <span class="logo-mini"><img class="logo_title" src="<?php echo base_url(); ?>assets/dist/img/ts.png"></span>
      <!-- logo for regular state and mobile devices -->
      <span class="logo-lg"><img class="logo_title_big" src="<?php echo base_url(); ?>assets/dist/img/ts.png">SMK TELKOM MALANG</span>
    </a>
    <!-- Header Navbar: style can be found in header.less -->
    <nav class="navbar navbar-static-top">
      <!-- Sidebar toggle button-->
      <a href="#" class="sidebar-toggle" data-toggle="offcanvas" role="button">
        <span class="sr-only">Toggle navigation</span>
      </a>

      <div class="navbar-custom-menu">
        <ul class="nav navbar-nav">
          <!-- User Account: style can be found in dropdown.less -->
          <li class="dropdown user user-menu">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">
              <img src="<?php echo base_url();?>assets/dist/img/user2-160x160.jpg" class="user-image" alt="User Image">
              <span class="hidden-xs"><?php echo $this->session->userdata('nama'); ?></span>
            </a>
            <ul class="dropdown-menu">
              <!-- User image -->
              <li class="user-header">
                <img src="<?php echo base_url();?>assets/dist/img/user2-160x160.jpg" class="img-circle" alt="User Image">

                <p>
                  <?php echo $this->session->userdata('nama'); ?> <br> <?php echo $this->session->userdata('status'); ?>
                <br><?php echo $this->session->userdata('nip'); ?>
                </p>
              </li>
              
              <!-- Menu Footer-->
              <li class="user-footer">
                <div class="pull-left">
                </div>
                <div class="pull-right">
                  <a href="<?php echo base_url()?>index.php/admins/keluar" class="btn btn-default btn-flat">Sign out</a>
                </div>
              </li>
            </ul>
          </li>
          <!-- Control Sidebar Toggle Button -->
          <li>
            <a href="#" data-toggle="control-sidebar"><i class="fa fa-gears"></i></a>
          </li>
        </ul>
      </div>
    </nav>
  </header>
  <!-- Left side column. contains the logo and sidebar -->
  <aside class="main-sidebar">
    <!-- sidebar: style can be found in sidebar.less -->
    <section class="sidebar">
      <!-- Sidebar user panel -->
      <div class="user-panel">
        <div class="pull-left image">
          <img src="<?php echo base_url();?>assets/dist/img/user2-160x160.jpg" class="img-circle" alt="User Image">
        </div>
        <div class="pull-left info">
          <p></p>
          <a href="#"><i class="fa fa-circle text-success"></i> Online</a>
        </div>
      </div>
      <!-- search form -->
      <form action="#" method="get" class="sidebar-form">
        <div class="input-group">
          <input type="text" name="q" class="form-control" placeholder="Search...">
              <span class="input-group-btn">
                <button type="submit" name="search" id="search-btn" class="btn btn-flat"><i class="fa fa-search"></i>
                </button>
              </span>
        </div>
      </form>
      <!-- /.search form -->
      <!-- sidebar menu: : style can be found in sidebar.less -->
      <ul class="sidebar-menu">
        <li class="header">MAIN NAVIGATION</li>
        <li><a href="<?php echo base_url(); ?>index.php/home/"><i class="fa fa-home"></i> <span>Home</span></a></li>
        <li><a href="<?php echo base_url(); ?>index.php/karyawan/"><i class="fa fa-desktop"></i> <span>Daftar Karyawan</span></a></li>
        <li><a href="<?php echo base_url(); ?>index.php/kelas/"><i class="fa fa-bar-chart-o"></i> <span>Daftar Kelas</span></a></li>
        <li><a href="<?php echo base_url(); ?>index.php/pelajaran/"><i class="fa fa-edit"></i> <span>Daftar Pelajaran</span></a></li>
        <li class="treeview">
          <a href="#">
            <i class="fa fa-table"></i> <span>Daftar Siswa</span>
            <span class="pull-right-container">
              <i class="fa fa-angle-left pull-right"></i>
            </span>
          </a>
          <ul class="treeview-menu">
            <li><a href="<?php echo base_url(); ?>index.php/siswa/kelas_10"><i class="fa fa-circle-o"></i> Kelas X</a></li>
            <li><a href="<?php echo base_url(); ?>index.php/siswa/kelas_11"><i class="fa fa-circle-o"></i> Kelas XI</a></li>
            <li><a href="<?php echo base_url(); ?>index.php/siswa/kelas_12"><i class="fa fa-circle-o"></i> Kelas XII</a></li>
          </ul>
        </li>
        <li><a href="<?php echo base_url(); ?>index.php/nilai/"><i class="fa fa-windows"></i> <span>Nilai</span></a></li>
        <li><a href=""><i class="fa fa-linux"></i> <span>Keuangan</span></a></li>
        <li><a href="<?php echo base_url(); ?>index.php/catatan/"><i class="fa fa-pencil"></i> <span>Catatan Siswa</span></a></li>
        <li class="header">ADMIN NAVIGATION</li>
        <li><a href="#"><i class="fa fa-gear"></i> <span>Setting</span></a></li>
      </ul>
    </section>
    <!-- /.sidebar -->
  </aside>

  <!-- Content Wrapper. Contains page content -->
  <!-- Content Wrapper. Contains page content -->
  <div class="content-wrapper">
    
    <!-- Content Header (Page header) -->
    <section class="content-header">
      <h1>
        SMK TELKOM MALANG
        <small>ATTITUDE IS EVERYTHING</small>
      </h1>
      <!-- <ol class="breadcrumb">
        <li><a href="#"><i class="fa fa-dashboard"></i> YPT</a></li>
        <li><a href="#">Charts</a></li>
        <li class="active">ChartJS</li>
      </ol> -->
    </section>

    <!-- Main content -->
    <section class="content">

      <?php $this->load->view($main_view); ?>
      
    </section>
    <!-- /.content -->
  </div>
  <!-- /.content-wrapper -->
  <footer class="main-footer">
    <div class="pull-right hidden-xs">
      <b>Version</b> 2.3.8
    </div>
    <strong>Copyright &copy; 2014-2016 <a href="http://almsaeedstudio.com">Almsaeed Studio</a>.</strong> All rights
    reserved.
  </footer>

  <!-- Control Sidebar -->
  <aside class="control-sidebar control-sidebar-dark">
    <!-- Create the tabs -->
    <ul class="nav nav-tabs nav-justified control-sidebar-tabs">
      <li><a href="#control-sidebar-home-tab" data-toggle="tab"><i class="fa fa-home"></i></a></li>
      <li><a href="#control-sidebar-settings-tab" data-toggle="tab"><i class="fa fa-gears"></i></a></li>
    </ul>
    <!-- Tab panes -->
    <div class="tab-content">
      <!-- Home tab content -->
      <div class="tab-pane" id="control-sidebar-home-tab">
        <h3 class="control-sidebar-heading">Recent Activity</h3>
        <ul class="control-sidebar-menu">
          <li>
            <a href="javascript:void(0)">
              <i class="menu-icon fa fa-birthday-cake bg-red"></i>

              <div class="menu-info">
                <h4 class="control-sidebar-subheading">Langdon's Birthday</h4>

                <p>Will be 23 on April 24th</p>
              </div>
            </a>
          </li>
          <li>
            <a href="javascript:void(0)">
              <i class="menu-icon fa fa-user bg-yellow"></i>

              <div class="menu-info">
                <h4 class="control-sidebar-subheading">Frodo Updated His Profile</h4>

                <p>New phone +1(800)555-1234</p>
              </div>
            </a>
          </li>
          <li>
            <a href="javascript:void(0)">
              <i class="menu-icon fa fa-envelope-o bg-light-blue"></i>

              <div class="menu-info">
                <h4 class="control-sidebar-subheading">Nora Joined Mailing List</h4>

                <p>nora@example.com</p>
              </div>
            </a>
          </li>
          <li>
            <a href="javascript:void(0)">
              <i class="menu-icon fa fa-file-code-o bg-green"></i>

              <div class="menu-info">
                <h4 class="control-sidebar-subheading">Cron Job 254 Executed</h4>

                <p>Execution time 5 seconds</p>
              </div>
            </a>
          </li>
        </ul>
        <!-- /.control-sidebar-menu -->

        <h3 class="control-sidebar-heading">Tasks Progress</h3>
        <ul class="control-sidebar-menu">
          <li>
            <a href="javascript:void(0)">
              <h4 class="control-sidebar-subheading">
                Custom Template Design
                <span class="label label-danger pull-right">70%</span>
              </h4>

              <div class="progress progress-xxs">
                <div class="progress-bar progress-bar-danger" style="width: 70%"></div>
              </div>
            </a>
          </li>
          <li>
            <a href="javascript:void(0)">
              <h4 class="control-sidebar-subheading">
                Update Resume
                <span class="label label-success pull-right">95%</span>
              </h4>

              <div class="progress progress-xxs">
                <div class="progress-bar progress-bar-success" style="width: 95%"></div>
              </div>
            </a>
          </li>
          <li>
            <a href="javascript:void(0)">
              <h4 class="control-sidebar-subheading">
                Laravel Integration
                <span class="label label-warning pull-right">50%</span>
              </h4>

              <div class="progress progress-xxs">
                <div class="progress-bar progress-bar-warning" style="width: 50%"></div>
              </div>
            </a>
          </li>
          <li>
            <a href="javascript:void(0)">
              <h4 class="control-sidebar-subheading">
                Back End Framework
                <span class="label label-primary pull-right">68%</span>
              </h4>

              <div class="progress progress-xxs">
                <div class="progress-bar progress-bar-primary" style="width: 68%"></div>
              </div>
            </a>
          </li>
        </ul>
        <!-- /.control-sidebar-menu -->

      </div>
      <!-- /.tab-pane -->
      <!-- Stats tab content -->
      <div class="tab-pane" id="control-sidebar-stats-tab">Stats Tab Content</div>
      <!-- /.tab-pane -->
      <!-- Settings tab content -->
      <div class="tab-pane" id="control-sidebar-settings-tab">
        <form method="post">
          <h3 class="control-sidebar-heading">General Settings</h3>

          <div class="form-group">
            <label class="control-sidebar-subheading">
              Report panel usage
              <input type="checkbox" class="pull-right" checked>
            </label>

            <p>
              Some information about this general settings option
            </p>
          </div>
          <!-- /.form-group -->

          <div class="form-group">
            <label class="control-sidebar-subheading">
              Allow mail redirect
              <input type="checkbox" class="pull-right" checked>
            </label>

            <p>
              Other sets of options are available
            </p>
          </div>
          <!-- /.form-group -->

          <div class="form-group">
            <label class="control-sidebar-subheading">
              Expose author name in posts
              <input type="checkbox" class="pull-right" checked>
            </label>

            <p>
              Allow the user to show his name in blog posts
            </p>
          </div>
          <!-- /.form-group -->

          <h3 class="control-sidebar-heading">Chat Settings</h3>

          <div class="form-group">
            <label class="control-sidebar-subheading">
              Show me as online
              <input type="checkbox" class="pull-right" checked>
            </label>
          </div>
          <!-- /.form-group -->

          <div class="form-group">
            <label class="control-sidebar-subheading">
              Turn off notifications
              <input type="checkbox" class="pull-right">
            </label>
          </div>
          <!-- /.form-group -->

          <div class="form-group">
            <label class="control-sidebar-subheading">
              Delete chat history
              <a href="javascript:void(0)" class="text-red pull-right"><i class="fa fa-trash-o"></i></a>
            </label>
          </div>
          <!-- /.form-group -->
        </form>
      </div>
      <!-- /.tab-pane -->
    </div>
  </aside>
  <!-- /.control-sidebar -->
  <!-- Add the sidebar's background. This div must be placed
       immediately after the control sidebar -->
  <div class="control-sidebar-bg"></div>
</div>
<!-- ./wrapper -->

<!-- jQuery 2.2.3 -->
<script src="<?php echo base_url();?>assets/plugins/jQuery/jquery-2.2.3.min.js"></script>
<script src="<?php echo base_url();?>assets/plugins/jQuery/jquery.js"></script>
<!-- Bootstrap 3.3.6 -->
<script src="<?php echo base_url();?>assets/bootstrap/js/bootstrap.min.js"></script>
<!-- DataTables -->
<script src="<?php echo base_url();?>assets/plugins/datatables/jquery.dataTables.min.js"></script>
<script src="<?php echo base_url();?>assets/plugins/datatables/dataTables.bootstrap.min.js"></script>
<!-- Select2 -->
<script src="<?php echo base_url();?>assets/plugins/select2/dist/js/select2.full.min.js"></script>
<!-- InputMask -->
<script src="<?php echo base_url();?>assets/plugins/input-mask/jquery.inputmask.js"></script>
<script src="<?php echo base_url();?>assets/plugins/input-mask/jquery.inputmask.date.extensions.js"></script>
<script src="<?php echo base_url();?>assets/plugins/input-mask/jquery.inputmask.extensions.js"></script>
<!-- date-range-picker -->
<script src="<?php echo base_url();?>assets/plugins/moment/min/moment.min.js"></script>
<script src="<?php echo base_url();?>assets/plugins/daterangepicker/daterangepicker.js"></script>
<!-- bootstrap datepicker -->
<script src="<?php echo base_url();?>assets/plugins/datepicker/dist/js/bootstrap-datepicker.min.js"></script>
<!-- bootstrap color picker -->
<script src="<?php echo base_url();?>assets/plugins/colorpicker/dist/js/bootstrap-colorpicker.min.js"></script>
<!-- bootstrap time picker -->
<script src="<?php echo base_url();?>assets/plugins/timepicker/bootstrap-timepicker.min.js"></script>
<!-- SlimScroll -->
<script src="<?php echo base_url();?>assets/plugins/slimScroll/jquery.slimscroll.min.js"></script>
<!-- iCheck 1.0.1 -->
<script src="<?php echo base_url();?>assets/plugins/iCheck/icheck.min.js"></script>
<!-- FastClick -->
<script src="<?php echo base_url();?>assets/plugins/fastclick/fastclick.js"></script>
<!-- AdminLTE App -->
<script src="<?php echo base_url();?>assets/dist/js/app.min.js"></script>
<!-- AdminLTE for demo purposes -->
<script src="<?php echo base_url();?>assets/dist/js/demo.js"></script>
<!-- page script -->
<script src="<?php echo base_url();?>assets/bootstrap/js/jquery.tableedit.js"></script>
<!-- notif alert delete -->
<script src="<?php echo base_url();?>assets/plugins/sweetalert/sweetalert.min.js"></script>
<script type="text/javascript">
  $(function () {
    $(".example1").DataTable();
    $('.example2').DataTable({
      "paging": true,
      "lengthChange": false,
      "searching": false,
      "ordering": true,
      "info": true,
      "autoWidth": false
    });
  });
</script>
<script type="text/javascript">
  $(function(){

    $.ajaxSetup({
      type:"post",
      cache:false,
      dataType: "json"
    })


    $(document).on("click","td",function(){
      $(this).find("span[class~='caption']").hide();
      $(this).find("input[class~='editor']").fadeIn().focus();
    });

    $(document).on("keydown",".editor",function(e){
      if(e.keyCode==13){
        var target=$(e.target);
        var value=target.val();
        var id=target.attr("data-id");
        var data={id:id,value:value};
        if(target.is(".field-uh1")){
          data.modul="uh1";
        }else if(target.is(".field-uh2")){
          data.modul="uh2";
        }else if(target.is(".field-uh3")){
          data.modul="uh3";
        }else if(target.is(".field-uh4")){
          data.modul="uh4";
        }else if(target.is(".field-uh5")){
          data.modul="uh5";
        }else if(target.is(".field-uts1")){
          data.modul="uts1";
        }else if(target.is(".field-uts2")){
          data.modul="uts2";
        }else if(target.is(".field-uas1")){
          data.modul="uas1";
        }else if(target.is(".field-uas2")){
          data.modul="uas2";
        }

        $.ajax({
          data:data,
          url:"<?php echo base_url('index.php/nilai/edit_nilai'); ?>",
          success: function(a){
           target.hide();
           target.siblings("span[class~='caption']").html(value).fadeIn();
          }
        })
        
      }
    });

    $(document).on("keydown",".editor",function(e){
      if(e.keyCode==13){
        var target=$(e.target);
        var value=target.val();
        var id=target.attr("data-id");
        var data={id:id,value:value};
        if(target.is(".field-catatan")){
          data.modul="catatan";
        }

        $.ajax({
          data:data,
          url:"<?php echo base_url('index.php/catatan/edit_catatan'); ?>",
          success: function(a){
           target.hide();
           target.siblings("span[class~='caption']").html(value).fadeIn();
          }
        })
        
      }
    });

    $(document).on("click",".hapus-karyawan",function(){
      var kode_karyawan=$(this).attr("data-id");
      swal({
        title:"Hapus Karyawan",
        text:"Yakin akan menghapus karyawan ini?",
        type: "warning",
        showCancelButton: true,
        confirmButtonText: "Hapus",
        closeOnConfirm: true,
      },
        function(){
         $.ajax({
          url:"<?php echo base_url('index.php/karyawan/delete'); ?>",
          data:{kode_karyawan:kode_karyawan},
          success: function(){
            $("tr[data-id='"+kode_karyawan+"']").fadeOut("fast",function(){
              $(this).remove();
            });
          }
         });
      });
    });

    $(document).on("click",".hapus-pelajaran",function(){
      var kode_pelajaran=$(this).attr("data-id");
      swal({
        title:"Hapus Pelajaran",
        text:"Yakin akan menghapus pelajaran ini?",
        type: "warning",
        showCancelButton: true,
        confirmButtonText: "Hapus",
        closeOnConfirm: true,
      },
        function(){
         $.ajax({
          url:"<?php echo base_url('index.php/pelajaran/delete'); ?>",
          data:{kode_pelajaran:kode_pelajaran},
          success: function(){
            $("tr[data-id='"+kode_pelajaran+"']").fadeOut("fast",function(){
              $(this).remove();
            });
          }
         });
      });
    });

    $(document).on("click",".hapus-data-nilai-matematika",function(){
      var id_nilai=$(this).attr("data-id-mtk");
      swal({
        title:"Hapus Data Nilai Siswa",
        text:"Yakin akan menghapus data nilai siswa ini?",
        type: "warning",
        showCancelButton: true,
        confirmButtonText: "Hapus",
        closeOnConfirm: true,
      },
        function(){
         $.ajax({
          url:"<?php echo base_url('index.php/nilai/delete_matematika'); ?>",
          data:{id_nilai:id_nilai},
          success: function(){
            $("tr[data-id-mtk='"+id_nilai+"']").fadeOut("fast",function(){
              $(this).remove();
            });
          }
         });
      });
    });

    $(document).on("click",".hapus-data-nilai-fisika",function(){
      var id_nilai=$(this).attr("data-id-fsk");
      swal({
        title:"Hapus Data Nilai Siswa",
        text:"Yakin akan menghapus data nilai siswa ini?",
        type: "warning",
        showCancelButton: true,
        confirmButtonText: "Hapus",
        closeOnConfirm: true,
      },
        function(){
         $.ajax({
          url:"<?php echo base_url('index.php/nilai/delete_fisika'); ?>",
          data:{id_nilai:id_nilai},
          success: function(){
            $("tr[data-id-fisika='"+id_nilai+"']").fadeOut("fast",function(){
              $(this).remove();
            });
          }
         });
      });
    });

    $(document).on("click",".hapus-data-catatan",function(){
      var id_catatan=$(this).attr("data-id");
      swal({
        title:"Hapus Data Catatan Siswa",
        text:"Yakin akan menghapus data catatan siswa ini?",
        type: "warning",
        showCancelButton: true,
        confirmButtonText: "Hapus",
        closeOnConfirm: true,
      },
        function(){
         $.ajax({
          url:"<?php echo base_url('index.php/catatan/delete'); ?>",
          data:{id_catatan:id_catatan},
          success: function(){
            $("tr[data-id='"+id_catatan+"']").fadeOut("fast",function(){
              $(this).remove();
            });
          }
         });
      });
    });

    $(document).on("click",".info-data-catatan",function(){
      var id_catatan=$(this).attr("info");
      swal({
        title:"Info Data Catatan Siswa",
        text:"Contoh pengisian :\n (1) .... (2) .... ",
        type: "info",
        confirmButtonText: false,
        closeOnConfirm: true,
      },
        function(){
         
      });
    });


  });

</script>
</body>
</html>
