@extends('layouts.dashboard')
@section('title', 'Edit Users')
@section('content')
           <div class="">
          <div class="page-title">
            <div class="title_left">
              <h3>Edit Users</h3>
            </div>

            <div class="title_right">
              <div class="col-md-5 col-sm-5 col-xs-12 form-group pull-right top_search">
                <div class="input-group">
                  <span class="input-group-btn">
                    </span>
                </div>
              </div>
            </div>
          </div>
          <div class="clearfix"></div>
          <div class="row">
            <div class="col-md-12 col-sm-12 col-xs-12">
              <div class="x_panel">
                <div class="x_title">
                  <h2>Form Design <small>different form elements</small></h2>
                  <ul class="nav navbar-right panel_toolbox">
                    <li><a class="collapse-link"><i class="fa fa-chevron-up"></i></a>
                    </li>
                    <li class="dropdown">
                      <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false"><i class="fa fa-wrench"></i></a>
                      <ul class="dropdown-menu" role="menu">
                        <li><a href="#">Settings 1</a>
                        </li>
                        <li><a href="#">Settings 2</a>
                        </li>
                      </ul>
                    </li>
                    <li><a class="close-link"><i class="fa fa-close"></i></a>
                    </li>
                  </ul>
                  <div class="clearfix"></div>
                </div>
                <div class="x_content">
                  <br />
                  <form id="demo-form2" data-parsley-validate class="form-horizontal form-label-left">

                   <div class="form-group">
                      <label class="control-label col-md-3 col-sm-3 col-xs-12" for="name">Name <span class="required"></span>
                        </label>
                      <div class="col-md-6 col-sm-6 col-xs-12">
                        <input type="text" id="name" name="name" required="required" class="form-control col-md-7 col-xs-12" value="{{$user->name}}">
                      </div>
                    </div>
                  <div class="form-group">
                      <label class="control-label col-md-3 col-sm-3 col-xs-12" for="email">Email <span class="required"></span>
                        </label>
                      <div class="col-md-6 col-sm-6 col-xs-12">
                        <input type="text" id="email" name="email" required="required" class="form-control col-md-7 col-xs-12" value="{{$user->email}}">
                      </div>
                    </div>
                   <div class="form-group">
                      <label class="control-label col-md-3 col-sm-3 col-xs-12" for="level">Level <span class="required"></span>
                        </label>
                      <div class="col-md-6 col-sm-6 col-xs-12">
                        <input type="text" id="level" name="level" required="required" class="form-control col-md-7 col-xs-12" value="{{$user->level}}">
                      </div>
                    </div>
                    <div class="ln_solid"></div>
                    <div class="form-group">
                      <div class="col-md-6 col-sm-6 col-xs-12 col-md-offset-3">
                        <button class="btn btn-success submit" id="btnUpdate">Update</button>
                        <a  class="btn btn-primary submit" href="list-users">Cancel</a>
                      </div>
                    </div>
                  </form>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
@endsection
@section('scripts')
 <script>
  $(document).ready(function(){
    // aktifkan class nav user
    $('#nav-dashboard').removeClass('active');
    $('#nav-list-visitors').removeClass('active');
    $('#nav-list-logs').removeClass('active');
    $('#nav-list-users').addClass('active');
  });
</script>
<script>
 $(document).ready(function(){
      
    });
    // ini adalah proses submit data menggunakan Ajax
    $("#btnUpdate").click(function(event) {
      // kasih ini dong biar gag hard reload
      event.preventDefault();
      $.ajax({
        url: '{{route("users.update",['id' => $user->id])}}', // url edit data
        dataType: 'JSON',
        type: 'PUT',
        contentType: 'application/x-www-form-urlencoded',
        data: $("#demo-form2").serialize(), // data tadi diserialize berdasarkan name
        success: function( data, textStatus, jQxhr ){
            console.log('status =>', textStatus);
            console.log('data =>', data);
            // clear validation error messsages
            $('#errMsg').addClass('hide');
            $('#errData').html('');
            // scroll up
            // $('html, body').animate({
            //     scrollTop: $("#nav-top").offset().top
            // }, 2000);
            // tampilkan pesan sukses
            showNotifSuccess();
            // kembali kelist book
            window.location.href = '{{route("page.list-users")}}'
        },
        error: function( data, textStatus, errorThrown ){
          var messages = jQuery.parseJSON(data.responseText);
            console.log( errorThrown );
            // $('html, body').animate({
            //     scrollTop: $("#nav-top").offset().top
            // }, 2000);
            // scroll up 
            // tampilkan pesan error
             $('#errData').html('');
          $('#errMsg').addClass('alert-warning');
          $('#errMsg').removeClass('hide');
          $.each(messages, function(i, val) {
            $('#errData').append('<p>'+ i +' : ' + val +'</p>')
            console.log(i,val);
          });          
          // jangan clear data
        }
      });
    });
    
    function showNotifSuccess(){
     new PNotify({
        title: 'Success!',
        text: 'That thing that you were trying to do worked.',
        type: 'success'
      });
	  }
</script>
    @endsection