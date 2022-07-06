<!DOCTYPE html>
<html>

<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="Lengkeng">
    <META NAME="keywords" CONTENT="LengKeng">
    <meta name="Author" content="LengKeng, E-mail: ohmygodvt95@gmail.com">
    <meta name="copyright" content="Copyright <?php echo date('Y');?> by LengKeng">
    <link rel="shortcut icon" type="image/png" href="<?php echo base_url();?>asset/images/favicon.png" />
    <title>Admin</title>
    <!-- Load CSS-->
    <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>asset/css/reset.min.css">
    <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>asset/css/bootstrap.min.css">
    <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>asset/css/bootstrap-theme.min.css">
    <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>asset/css/font-awesome.min.css">
    <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>asset/css/sweetalert.min.css">
    <!-- Load customize style -->
    <link href="http://allfont.net/css/?fonts=bebas-neue" rel="stylesheet" type="text/css">
    <link href="http://fonts.googleapis.com/css?family=Raleway:400,500" rel="stylesheet" type="text/css">
    <link rel="stylesheet" type="text/css" href="<?php echo base_url();?>asset/css/style1.css">
    <style type="text/css">
        * {
            transition: 0.1s;
        }
        .head div{
            font-weight: bolder;
            text-transform: uppercase;
            border-bottom: 4px solid gray;
        }
        .dt div{
            line-height: 50px;
            border-bottom: 1px solid rgba(0,0,0, 0.4);
        }
        .btn{
            width: 100%!important;
        }
        /*footer*/
        footer{min-height:80px;background-color:#254F75;}
        footer h3{font-weight:100;font-family:"Roboto";font-size:20px;color:#fff;line-height:80px;}

    </style>
</head>

<body>
    <!-- menu-no-logged -->
    <?php $this->load->view('module/menu/menu-no-login');?>
    <div class="container" style="margin-top: 50px; margin-bottom: 50px;">
        <h1 class="text-center">Báo lỗi của người dùng</h1>
        <hr>
        <div role="tabpanel">
            <!-- Nav tabs -->
            <ul class="nav nav-tabs" role="tablist">
                <li role="presentation" class="active">
                    <a href="#new" aria-controls="home" role="tab" data-toggle="tab">Lỗi mới <sup><span class="badge"><?php echo count($data_new); ?></span> </sup></a>
                </li>
                <li role="presentation">
                    <a href="#seen" aria-controls="tab" role="tab" data-toggle="tab">Đã xem qua <sup><span class="badge"><?php echo count($data_seen); ?></span> </a>
                </li>
                <li role="presentation">
                    <a href="#done" aria-controls="tab" role="tab" data-toggle="tab">Đã giải quyết <sup><span class="badge"><?php echo count($data_done); ?></span> </a>
                </li>
            </ul>

            <!-- Tab panes -->
            <div class="tab-content">
                <div role="tabpanel" class="tab-pane active" id="new">
                    <table class="table table-hover">
                        <thead>
                            <tr>
                                <th>Mã</th>
                                <th>Người báo</th>
                                <th>Thời gian</th>
                                <th>Loại lỗi</th>
                                <th>Thuộc tuyến</th>
                                <th>Hành động</th>
                            </tr>
                        </thead>
                        <tbody>
                            <?php

                                    foreach ($data_new as $key) {
                                        echo '<tr>
                                    ';
                                        $item = json_decode($key->stringdata, FALSE);

                                        echo '<td>'.$key->_id.'</td>
                                        ';
                                        echo '<td>'.$item->userEmail.'</td>
                                        ';
                                        echo '<td>'.$key->time_stamp.'</td>
                                        ';
                                        echo '<td>'.$item->reportType.'</td>
                                        ';
                                        echo '<td>'.$item->busLine->code.'</td>
                                        ';
                                        if($key->status == '0'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" class="btn btn-danger"><i class="fa fa-fw fa-wrench"></i>Lỗi mới</a></td>';
                                        }
                                        else
                                        if($key->status == '2'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" title="" class="btn btn-success"><i class="fa fa-fw fa-check"></i>Đã giải quyết</a></td>';
                                        }
                                        else
                                        if($key->status == '1'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" title="" class="btn btn-warning"><i class="fa fa-fw fa-eye"></i>Đã xem qua</a></td>';
                                        }
                                        echo '</tr>';
                                    }

                                ?>
                        </tbody>
                    </table>
                </div>
                <div role="tabpanel" class="tab-pane" id="seen">
                            <table class="table table-hover">
                        <thead>
                            <tr>
                                <th>Mã</th>
                                <th>Người báo</th>
                                <th>Thời gian</th>
                                <th>Loại lỗi</th>
                                <th>Thuộc tuyến</th>
                                <th>Hành động</th>
                            </tr>
                        </thead>
                        <tbody>
                            <?php

                                    foreach ($data_seen as $key) {
                                        echo '<tr>
                                    ';
                                        $item = json_decode($key->stringdata, FALSE);

                                        echo '<td>'.$key->_id.'</td>
                                        ';
                                        echo '<td>'.$item->userEmail.'</td>
                                        ';
                                        echo '<td>'.$key->time_stamp.'</td>
                                        ';
                                        echo '<td>'.$item->reportType.'</td>
                                        ';
                                        echo '<td>'.$item->busLine->code.'</td>
                                        ';
                                        if($key->status == '0'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" class="btn btn-danger"><i class="fa fa-fw fa-wrench"></i>Lỗi mới</a></td>';
                                        }
                                        else
                                        if($key->status == '2'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" title="" class="btn btn-success"><i class="fa fa-fw fa-check"></i>Đã giải quyết</a></td>';
                                        }
                                        else
                                        if($key->status == '1'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" title="" class="btn btn-warning"><i class="fa fa-fw fa-eye"></i>Đã xem qua</a></td>';
                                        }
                                        echo '</tr>';
                                    }

                                ?>
                        </tbody>
                    </table>
                </div>
                <div role="tabpanel" class="tab-pane" id="done">
                     <table class="table table-hover">
                        <thead>
                            <tr>
                                <th>Mã</th>
                                <th>Người báo</th>
                                <th>Thời gian</th>
                                <th>Loại lỗi</th>
                                <th>Thuộc tuyến</th>
                                <th>Hành động</th>
                            </tr>
                        </thead>
                        <tbody>
                            <?php

                                    foreach ($data_done as $key) {
                                        echo '<tr>
                                    ';
                                        $item = json_decode($key->stringdata, FALSE);

                                        echo '<td>'.$key->_id.'</td>
                                        ';
                                        echo '<td>'.$item->userEmail.'</td>
                                        ';
                                        echo '<td>'.$key->time_stamp.'</td>
                                        ';
                                        echo '<td>'.$item->reportType.'</td>
                                        ';
                                        echo '<td>'.$item->busLine->code.'</td>
                                        ';
                                        if($key->status == '0'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" class="btn btn-danger"><i class="fa fa-fw fa-wrench"></i>Lỗi mới</a></td>';
                                        }
                                        else
                                        if($key->status == '2'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" title="" class="btn btn-success"><i class="fa fa-fw fa-check"></i>Đã giải quyết</a></td>';
                                        }
                                        else
                                        if($key->status == '1'){
                                            echo '<td><a href="'.site_url('report/xuly/'.$key->_id).'" title="" class="btn btn-warning"><i class="fa fa-fw fa-eye"></i>Đã xem qua</a></td>';
                                        }
                                        echo '</tr>';
                                    }

                                ?>
                        </tbody>
                    </table>
                </div>
            </div>
        </div>
    </div>
    <?php $this->load->view('module/footer');?>
            <!-- Load jQuery -->
            <script type="text/javascript" src="<?php echo base_url();?>asset/js/jquery.min.js"></script>
            <script type="text/javascript" src="<?php echo base_url();?>asset/js/bootstrap.min.js"></script>
            <script type="text/javascript" src="<?php echo base_url();?>asset/js/sweetalert.min.js"></script>
            <!-- loading -->
            <script type="text/javascript" src="<?php echo base_url();?>asset/js/loading.js"></script>
</body>

</html>