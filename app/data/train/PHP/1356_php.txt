<link rel="stylesheet" type="text/css" href="<?php echo base_url(); ?>assets/css/pages/datatables.css">
<link href="<?php echo base_url(); ?>assets/vendors/bootstrapvalidator/css/bootstrapValidator.min.css" rel="stylesheet">
<script type="text/javascript" src="<?php echo base_url(); ?>assets/vendors/bootstrapvalidator/js/bootstrapValidator.min.js"></script>

<style>
        @media(max-width: 1024px)
        {
            .radio-inline + .radio-inline, .checkbox-inline + .checkbox-inline {
                margin-top: 0;
                margin-left: 8px;
            }
        }
    </style>


<div class="row">
  <div class="col-md-12">
                    <!-- First Basic Table strats here-->
        <div class="panel">
            <?php if ($status=='2') { ?>
                  <div class="panel-heading" style="background-color: #2980b9;">
                      <h3 class="panel-title">
                          <i class="ti-layout-cta-left"></i> Disetujui
                    </h3>
                  </div>
                <?php }else if ($status=='1'){ ?>
                  <div class="panel-heading" style="background-color: #f1c40f;">
                      <h3 class="panel-title">
                          <i class="ti-layout-cta-left"></i> Proses
                    </h3>
                  </div>
                <?php  }else if ($status=='3'){ ?>
                    <div class="panel-heading" style="background-color: #e74c3c;">
                      <h3 class="panel-title">
                          <i class="ti-layout-cta-left"></i> Tidak Disetujui
                    </h3>
                  </div>
                <?php } ?>


            
            <div class="panel-body">
              <div class="col-lg-12">
                 <?php if ($status=='2') { ?>
                  <p>Anda dapat mencetak rekomendasi kecamatan</p>
                <?php }else if ($status=='1') { ?>
                  <p>Data Anda belum diproses</p>
                <?php  }else if ($status=='3') { ?>
                  <p>Data Anda tidak disetujui oleh kecamatan, silahkan periksa kembali data anda. dan lakukan update.</p>
                <?php } ?>
              </div>
                   

            </div>
        </div>
    </div>
</div>
    

<form method="post" class="form-horizontal p-10" role="form">

<div class="row">
  <div class="col-lg-12">
                    <!-- First Basic Table strats here-->
        <div class="panel">
            <div class="panel-heading">
                <h3 class="panel-title">
                    <i class="ti-layout-cta-left"></i> Data Pemohon
               </h3>
            </div>
            <div class="panel-body">

            
                <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">NIK Pemohon</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="nik_pemohon" id="nik_pemohon" placeholder="NIK Pemohon" value="<?php echo isset($nik_pemohon)?$nik_pemohon:''; ?>" readonly>
                    </div>
                  </div>


                 <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Nama Pemohon</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="nama_pemohon" id="nama_pemohon" placeholder="Nama Pemohon" value="<?php echo isset($nama_pemohon)?$nama_pemohon:''; ?>" readonly>
                    </div>
                  </div> 
                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Tempat Lahir</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="tempat_lahir" id="tempat_lahir" placeholder="Tempat Lahir" value="<?php echo isset($tempat_lahir)?$tempat_lahir:''; ?>" readonly>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Tanggal Lahir</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control tanggal" name="tgl_lahir" id="tgl_lahir" placeholder="Tanggal Lahir" data-date-format="dd-mm-yyyy" value="<?php echo isset($tgl_lahir)?$tgl_lahir:''; ?>" readonly>
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Pekerjaan</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="pekerjaan" id="pekerjaan" placeholder="Pekerjaan" value="<?php echo isset($pekerjaan)?$pekerjaan:''; ?>" readonly>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">No. Telp</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="no_telp" id="no_telp" placeholder="No. Telp" value="<?php echo isset($no_telp)?$no_telp:''; ?>" readonly>
                    </div>
                  </div> 

                   <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Negara</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="negara_pemohon" id="negara_pemohon" placeholder="Negara" value="<?php echo isset($negara_pemohon)?$negara_pemohon:''; ?>" readonly>
                    </div>
                  </div> 



                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Alamat</label>
                    <div class="col-md-9">
                      <textarea rows="3" class="form-control resize_vertical" name="alamat" id="alamat" placeholder="Alamat" readonly><?php echo isset($alamat)?$alamat:''; ?></textarea>
                    </div>
                  </div>          

            </div>
        </div>
    </div>
</div>

<div class="row">
  <div class="col-lg-12">
                    <!-- First Basic Table strats here-->
        <div class="panel">
            <div class="panel-heading">
                <h3 class="panel-title">
                    <i class="ti-layout-cta-left"></i> Data Perusahaan
               </h3>
            </div>
            <div class="panel-body">

            

              

              <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Nama Usaha</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="nama_usaha" id="nama_usaha" placeholder="Nama Usaha" value="<?php echo isset($nama_usaha)?$nama_usaha:''; ?>" readonly >
                    </div>
                  </div>

              <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Jenis Usaha</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="jenis_usaha" id="jenis_usaha" placeholder="Jenis Usaha" value="<?php echo isset($jenis_usaha)?$jenis_usaha:''; ?>" readonly >
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Ukuruan Luas</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="ukuran_luas_usaha" id="ukuran_luas_usaha" placeholder="Ukuran Usaha" value="<?php echo isset($ukuran_luas_usaha)?$ukuran_luas_usaha:''; ?>" readonly >
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Lokasi</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="lokasi_usaha" id="lokasi_usaha" placeholder="Lokasi Usaha" value="<?php echo isset($lokasi_usaha)?$lokasi_usaha:''; ?>" readonly >
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Status Bangunan</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="status_bangunan_tempat_usaha" id="status_bangunan_tempat_usaha" placeholder="Status Bangunan Tempat Usaha" value="<?php echo isset($status_bangunan_tempat_usaha)?$status_bangunan_tempat_usaha:''; ?>" readonly >
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">NPWPD</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="npwpd" id="npwpd" placeholder="NPWPD" value="<?php echo isset($npwpd)?$npwpd:''; ?>" readonly >
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Klasifikasi Usaha</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="klasifikasi_usaha" id="klasifikasi_usaha" placeholder="Klasifikasi Usaha" value="<?php echo isset($klasifikasi_usaha)?$klasifikasi_usaha:''; ?>" readonly >
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Retribusi Pertahun Fiskal</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="retribusi_perthn_f" id="retribusi_perthn_f" placeholder="Retribusi Pertahun Fiskal" value="<?php echo isset($retribusi_perthn_f)?$retribusi_perthn_f:''; ?>" >
                    </div>
                  </div>

            </div>
        </div>
    </div>
</div>


<div class="row">
  <div class="col-lg-12">
                    <!-- First Basic Table strats here-->
        <div class="panel">
            <div class="panel-heading">
                <h3 class="panel-title">
                    <i class="ti-layout-cta-left"></i> Jenis Pendaftaran
               </h3>
            </div>
            <div class="panel-body">

              <div class="form-group p-10">
                    <label class="control-label col-md-8" for="text">&nbsp;</label>
                    <div class="col-md-4">
                      <div class="col-md-6">
                        <b>Baru</b>
                      </div>
                      <div class="col-md-6">
                        <b>Ulang</b>
                      </div>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">1. Jenis Pendaftaran </label>
                    <div class="col-md-4">
                      <?php if ($jenis_permohonan=='baru') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="jenis_permohonan" class="radio-blue" value="baru" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="jenis_permohonan" class="radio-blue" value="lama" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div>  
 

            </div>
        </div>
    </div>
</div>

<div class="row">
  <div class="col-lg-12">
                    <!-- First Basic Table strats here-->
        <div class="panel">
            <div class="panel-heading">
                <h3 class="panel-title">
                    <i class="ti-layout-cta-left"></i> Syarat Umum
               </h3>
            </div>
            <div class="panel-body">

              <div class="form-group p-10">
                    <label class="control-label col-md-8" for="text">&nbsp;</label>
                    <div class="col-md-4">
                      <div class="col-md-6">
                        <b>Ada</b>
                      </div>
                      <div class="col-md-6">
                        <b>Tidak Ada</b>
                      </div>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">1.  Fotokopi KTP ( 1 (satu) lembar) </label>
                    <div class="col-md-4">
                      <?php if ($ktp=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="ktp" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="ktp" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div>  

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">2. Fotocopy Tanda Bukti Hak Atas Tanah     </label>
                    <div class="col-md-4">
                      <?php if ($fc_hak_tanah=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="fc_hak_tanah" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="fc_hak_tanah" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div>   

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">3. Surat Pengantar dari kelurahan/desa   </label>
                    <div class="col-md-4">
                      <?php if ($sp_desa=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="sp_desa" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="sp_desa" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">4. Surat Permohonan bermaterai 6000     </label>
                    <div class="col-md-4">
                      <?php if ($sp_materai=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="sp_materai" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="sp_materai" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">5. Denah Lokasi </label>
                    <div class="col-md-4">
                      <?php if ($denah_lokasi=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="denah_lokasi" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="denah_lokasi" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">6. Pas Foto Berwarna 3 x 4 ( 3 (tiga) lembar)    </label>
                    <div class="col-md-4">
                      <?php if ($foto=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="foto" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="foto" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">7. Fotocopy PBB  </label>
                    <div class="col-md-4">
                      <?php if ($fc_pbb=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="fc_pbb" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="fc_pbb" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div> 

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">8. Rekomendasi dari UPTD Puskesmas setempat  </label>
                    <div class="col-md-4">
                      <?php if ($rekom_uptd=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="rekom_uptd" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="rekom_uptd" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div>      

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">9. Gambar Bangunan</label>
                    <div class="col-md-4">
                      <?php if ($gambar_bangunan=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="gambar_bangunan" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="gambar_bangunan" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div>  

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">10.  Instalasi air, listrik dan telepon</label>
                    <div class="col-md-4">
                      <?php if ($instalasi_air=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="instalasi_air" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="instalasi_air" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">11.  Rekomendasi Lurah/Kepala Desa setempat</label>
                    <div class="col-md-4">
                      <?php if ($rekom_desa=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="rekom_desa" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="rekom_desa" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                    </div>
                  </div>

            </div>
        </div>
    </div>
</div>

<div class="row">
  <div class="col-lg-12">
                    <!-- First Basic Table strats here-->
        <div class="panel">
            <div class="panel-heading">
                <h3 class="panel-title">
                    <i class="ti-layout-cta-left"></i> Syarat Tambahan Pendaftaran Ulang 
               </h3>
            </div>
            <div class="panel-body">

              <div class="form-group p-10">
                    <label class="control-label col-md-8" for="text">&nbsp;</label>
                    <div class="col-md-4">
                      <div class="col-md-6">
                        <b>Ada</b>
                      </div>
                      <div class="col-md-6">
                        <b>Tidak Ada</b>
                      </div>
                    </div>
                  </div> 

                  

                  <div class="form-group p-10">
                    <label class="col-md-8" for="text">1.  SIUP Asli    </label>
                    <div class="col-md-4">
                    <?php if ($siup_asli=='ada') { ?>
                      
                      <div class="col-md-6">
                        <input type="radio" name="siup_asli" class="radio-blue" value="ada" checked="true">
                      </div>
                      <div class="col-md-6">
                        &nbsp;
                      </div>

                    <?php }else{ ?>
                        
                        <div class="col-md-6">
                        &nbsp;
                      </div>
                      <div class="col-md-6">
                        <input type="radio" name="siup_asli" class="radio-blue" value="tidak ada" checked="true">
                      </div>

                      <?php } ?>
                  </div>  

                 

            </div>
        </div>
    </div>
</div>

<div class="row">
  <div class="col-lg-12">
                    <!-- First Basic Table strats here-->
        <div class="panel">
            <div class="panel-heading">
                <h3 class="panel-title">
                    <i class="ti-layout-cta-left"></i> Verifikasi dan Data Kecamatan
               </h3>
            </div>
            <div class="panel-body">

             <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Tanggal Register</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control tanggal" name="tgl_register" id="tgl_register" placeholder="Tanggal Register" data-date-format="dd-mm-yyyy" value="<?php echo isset($tgl_register)?$tgl_register:''; ?>" readonly>
                    </div>
                  </div>

              <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">No. Registrasi</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="no_register" id="no_register" placeholder="No. Registrasi" value="<?php echo isset($no_register)?$no_register:''; ?>" <?php if ($action=='update') { echo 'readonly'; } ?>>
                    </div>
                  </div>

              <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Nama Petugas Verifikasi</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="nama_petugas_verifikasi" id="nama_petugas_verifikasi" placeholder="Nama Petugas Verifikasi" value="<?php echo isset($nama_petugas_verifikasi)?$nama_petugas_verifikasi:''; ?>" readonly>
                    </div>
                  </div>

                 <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Tanggal Verifikasi</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control tanggal" name="tgl_verifikasi" id="tgl_verifikasi" placeholder="Tanggal Verifikasi" data-date-format="dd-mm-yyyy" value="<?php echo isset($tgl_verifikasi)?$tgl_verifikasi:''; ?>" readonly>
                    </div>
                  </div> 


                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">Nama Camat</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="nama_camat" id="nama_camat" placeholder="Nama Camat" value="<?php echo isset($nama_camat)?$nama_camat:''; ?>" readonly>
                    </div>
                  </div>

                  <div class="form-group p-10">
                    <label class="control-label col-md-3" for="text">NIP Camat</label>
                    <div class="col-md-9">
                      <input type="text" class="form-control" name="nip_camat" id="nip_camat" placeholder="NIP Camat" value="<?php echo isset($nip_camat)?$nip_camat:''; ?>" readonly>
                    </div>
                  </div>

                            

            </div>
        </div>
    </div>
</div>

</form>



<div class="col-md-6">
	&nbsp;
</div>
<?php if ($status=='2') { ?>
    <div class="col-md-2">
    <a href='#' class="btn btn-lg btn-primary" onclick="printsurat('<?php echo $id ?>')" ><i class='fa fa-print'></i> Izin</a>

  
</div>
<?php }else{ ?>
  <div class="col-md-2">
  &nbsp;
</div>
<?php  } ?>

<div class="col-md-2">
    <a href='#' class="btn btn-lg btn-primary" onclick="formulir('<?php echo $id ?>')" ><i class='fa fa-print'></i> Formulir</a>
</div>
<div class="col-md-2">
	<a href="<?php echo site_url($this->controller) ?>"> <button style="border-radius: 8;" id="reset" type="button" class="btn btn-lg btn-danger">Kembali</button></a>
</div>






<?php 
$this->load->view($this->controller.'_status_view_js');
?>