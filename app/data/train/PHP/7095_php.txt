<!DOCTYPE html>
<html>
<head>
	<title></title>

	<meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">

    <title>Admin ~ Diaspora Indonesia Network</title>

    <link rel="stylesheet" type="text/css" href=" <?php echo base_url('assets/css/bootstrap.min.css') ?> ">
    <link rel="stylesheet" type="text/css" href=" <?php echo base_url('assets/css/font-awesome.min.css') ?> ">

    <link rel="stylesheet" type="text/css" href=" <?php echo base_url('assets/css/select2.min.css') ?> " />

    <link rel="stylesheet" type="text/css" href=" <?php echo base_url('assets/css/bootstrap-datetimepicker.min.css') ?> " />

    <link rel="stylesheet" type="text/css" href=" <?php echo base_url('assets/css/AdminLTE.min.css') ?> " />
    <link rel="stylesheet" type="text/css" href=" <?php echo base_url('assets/css/skins/_all-skins.min.css') ?> " />

</head>
<body class="hold-transition skin-blue sidebar-mini" style="margin:0; padding:0;">

	<div class="wrapper">

        <?php $this->load->view('modules/header') ?>

        <?php echo $sidebar; ?>

        <?php echo $content; ?>

        <?php $this->load->view('modules/footer') ?>

        <!-- <?php echo $modals; ?> -->

    </div><!-- ./wrapper -->

    <script type="text/javascript" src="<?php echo base_url('assets/js/jquery.min.js'); ?>"></script>
    <script type="text/javascript" src="<?php echo base_url('assets/js/jquery-ui.min.js'); ?>"></script>

    <script type="text/javascript" src="<?php echo base_url('assets/js/select2.full.min.js'); ?>"></script>

    <script type="text/javascript" src="<?php echo base_url('assets/js/bootstrap.min.js'); ?>"></script>
    <script type="text/javascript" src="<?php echo base_url('assets/js/moment.js'); ?>"></script>
    <script type="text/javascript" src="<?php echo base_url('assets/js/bootstrap-datetimepicker.min.js'); ?>"></script>


    <script src="https://cdn.ckeditor.com/4.4.3/standard/ckeditor.js"></script>
    <script type="text/javascript" src="<?php echo base_url('assets/js/adminlte.min.js'); ?>"></script>

    <script type="text/javascript">
        $(function () {
            CKEDITOR.replace('editor1');
            $(".select2").select2();

            $('.tanggal-datepicker').datetimepicker({
                format: 'YYYY-MM-DD'
            });

            $('.waktu-datepicker').datetimepicker({
                format: 'hh:mm'
            });
        });
    </script>


</body>
</html>