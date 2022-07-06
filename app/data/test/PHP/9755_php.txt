@extends('layouts.default')
@section('title')Edit Pelayanan Pendidikan @endsection
@push('style')
<link href="{{ url('admin') }}/plugins/bootstrap-validator/bootstrapValidator.min.css" rel="stylesheet">
@endpush
@push('javascript')
<script src="{{ url('admin') }}/plugins/bootstrap-wizard/jquery.bootstrap.wizard.min.js"></script>
<script src="{{ url('admin') }}/plugins/bootstrap-validator/bootstrapValidator.min.js"></script>
<script src="{{ url('admin') }}/js/demo/form-wizard.js"></script>

@endpush
@section('content')
<?php $path = 'pendidikan' ?>

<!--CONTENT CONTAINER-->
<!--===================================================-->
<div id="content-container">

    <!--Page Title-->
    <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
    <div id="page-title">
        <h1 class="page-header text-overflow">Edit Data Pelayanan Pendidikan</h1>
    </div>
    <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
    <!--End page title-->

    <!--Breadcrumb-->
    <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
    <ol class="breadcrumb">
        <li><a href="{{ url('/') }}">Home</a></li>
        <li><a href="{{ url('pendidikan') }}">Pelayanan Pendidikan</a></li>
        <li class="active">Edit Data</li>
    </ol>
    <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
    <!--End breadcrumb-->

    <!--Page content-->
    <!--===================================================-->
    <div id="page-content">

        <div class="row">
            <div class="col-lg-12">
                <div class="panel">

                    <!-- Form wizard with Validation -->
                    <!--===================================================-->
                    <div id="demo-bv-wz">
                        <div class="wz-heading">

                            <!--Progress bar-->
                            <div class="progress progress-sm progress-light-base">
                                <div class="progress-bar progress-bar-primary"></div>
                            </div>

                            <!--Nav-->
                            <ul class="wz-icon-bw wz-nav-off">
                                <li class="col-xs-2">
                                    <a data-toggle="tab" href="#demo-bv-tab1" title="Description">
                                        <span class="icon-wrap icon-wrap-sm icon-circle bg-mint">
                                        <i class="wz-icon fa fa-th-list fa-2x"></i>
                                        <i class="wz-icon-done fa fa-check fa-2x"></i>
                                    </span>
                                    </a>
                                </li>
                                <li class="col-xs-2">
                                    <a data-toggle="tab" href="#demo-bv-tab2" title="Timeline">
                                        <span class="icon-wrap icon-wrap-sm icon-circle bg-info">
                                        <i class="wz-icon fa fa-calendar fa-2x"></i>
                                        <i class="wz-icon-done fa fa-check fa-2x"></i>
                                    </span>
                                    </a>
                                </li>
                                <li class="col-xs-2">
                                    <a data-toggle="tab" href="#demo-bv-tab3" title="Dokumen">
                                        <span class="icon-wrap icon-wrap-sm icon-circle bg-warning">
                                        <i class="wz-icon fa fa-file-text-o fa-2x"></i>
                                        <i class="wz-icon-done fa fa-check fa-2x"></i>
                                    </span>
                                    </a>
                                </li>
                                <li class="col-xs-2">
                                    <a data-toggle="tab" href="#demo-bv-tab4" title="Latar Belakang">
                                        <span class="icon-wrap icon-wrap-sm icon-circle bg-primary">
                                        <i class="wz-icon fa fa-picture-o fa-2x"></i>
                                        <i class="wz-icon-done fa fa-check fa-2x"></i>
                                    </span>
                                    </a>
                                </li>
                                <li class="col-xs-2">
                                    <a data-toggle="tab" href="#demo-bv-tab5" title="Evaluasi">
                                        <span class="icon-wrap icon-wrap-sm icon-circle bg-danger">
                                        <i class="wz-icon fa fa-clipboard fa-2x"></i>
                                        <i class="wz-icon-done fa fa-check fa-2x"></i>
                                    </span>
                                    </a>
                                </li>
                                <li class="col-xs-2">
                                    <a data-toggle="tab" href="#demo-bv-tab6" title="Submit">
                                        <span class="icon-wrap icon-wrap-sm icon-circle bg-success">
                                        <i class="wz-icon fa fa-check-square-o fa-2x"></i>
                                        <i class="wz-icon-done fa fa-check fa-2x"></i>
                                    </span>
                                    </a>
                                </li>
                            </ul>
                        </div>

                        <!--Form-->
                        {{ Form::model($model, array('route' => array('pendidikan.update', $model->id),'files'=> true,'id'=>'demo-bv-wz-form','class'=>'form-horizontal', 'method' => 'PUT')) }}
                            <input type="hidden" name="_token" value="{{ csrf_token() }}">
                            <div class="panel-body">
                                <div class="tab-content">

                                    <!--First tab-->
                                    @include('form.description')
                                    <!--Second tab-->
                                    @include('form.timeline')

                                    <!--Third tab-->
                                    <div id="demo-bv-tab3" class="tab-pane">
                                        <div class="form-group">
                                            <label class="col-lg-3 control-label">Perjanjian kerjasama dengan Univ/Lembaga</label>
                                            <div class="col-lg-3">
    											<input accept=".doc, .docx,.pdf" type="file" class="form-control" name="doc_kerjasama">
                                            </div>
                                            <div class="col-lg-4">
    											<input type="text" class="form-control" value="{{ $model->doc_kerjasama }}" disabled />
                                            </div>
                                        </div>
                                        <div class="form-group">
                                            <label class="col-lg-3 control-label">Rencana Anggaran Biaya </label>
                                            <div class="col-lg-3">
                                                <input accept=".doc, .docx,.pdf" type="file" class="form-control" name="doc_anggaran">
                                            </div>
                                            <div class="col-lg-4">
    											<input type="text" class="form-control" value="{{ $model->doc_anggaran }}" disabled />
                                            </div>
                                        </div>
                                        <div class="form-group">
                                            <label class="col-lg-3 control-label">Dokumen Manajemen Resiko  </label>
                                            <div class="col-lg-3">
                                                <input accept=".doc, .docx,.pdf" type="file" class="form-control" name="doc_resiko">
                                            </div>
                                            <div class="col-lg-4">
    											<input type="text" class="form-control" value="{{ $model->doc_resiko }}" disabled />
                                            </div>
                                        </div>
                                        <div class="form-group">
                                            <label class="col-lg-3 control-label">TOR Term Of Reference </label>
                                            <div class="col-lg-3">
                                                <input accept=".doc, .docx,.pdf" type="file" class="form-control" name="doc_tor">
                                            </div>
                                            <div class="col-lg-4">
    											<input type="text" class="form-control" value="{{ $model->doc_tor }}" disabled />
                                            </div>
                                        </div>
                                        <div class="form-group">
                                            <label class="col-lg-3 control-label">Laporan Akhir </label>
                                            <div class="col-lg-3">
                                                <input accept=".doc, .docx,.pdf" type="file" class="form-control" name="doc_laporan">
                                            </div>
                                            <div class="col-lg-4">
    											<input type="text" class="form-control" value="{{ $model->doc_laporan }}" disabled />
                                            </div>
                                        </div>
                                        <div class="form-group">
                                            <label class="col-lg-3 control-label">Laporan Evaluasi </label>
                                            <div class="col-lg-3">
                                                <input accept=".doc, .docx,.pdf" type="file" class="form-control" name="doc_evaluasi">
                                            </div>
                                            <div class="col-lg-4">
    											<input type="text" class="form-control" value="{{ $model->doc_evaluasi }}" disabled />
                                            </div>
                                        </div>
                                    </div>

                                    <!--Fourth tab-->
                                    @include('form.latar_belakang')
                                    <!--Fifth tab-->
                                    @include('form.evaluasi')
                                    <!--Sixth tab-->
                                    @include('form.submit')

                                </div>
                            </div>

                            <!--Footer button-->
                            <div class="panel-footer text-right">
                                <div class="box-inline">
                                    <button type="button" class="previous btn btn-primary">Previous</button>
                                    <button type="button" class="next btn btn-primary">Next</button>
                                    <button type="submit" class="finish btn btn-success">Finish</button>
                                </div>
                            </div>
                        {{ Form::close() }}
                    </div>
                    <!--===================================================-->
                    <!-- End Form wizard with Validation -->

                </div>
            </div>
        </div>
    </div>
</div>
@endsection
