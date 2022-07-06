 <script src='<?php echo base_url()?>assets/js/tinymce/tinymce.min.js'></script>
  <script>
  tinymce.init({
    selector: '#myartikel'
  });
  </script>
  <form action="<?php echo base_url('admin/Cartikel_g/proses_add_artikel') ?>" method="post" enctype="multipart/form-data">
  <div class="col-md-8 whitebox">
  <h3 class="container">Tambah Artikel</h3>
  <hr/>
      <input type="hidden" name="id_user" value="">
            <div class="form-group bts-ats">
              <label class="col-sm-2 control-label">Judul</label>
                <div class="col-sm-10">
                  <input type="text" name="judul_artikel" class="form-control" placeholder="judul">
                </div>
                <div class="sambungfloat"></div>
            </div>
            <div class="col-md-12">
    			   <textarea id="myartikel" name="isi_artikel" rows="15"></textarea>
  			     </div>
          	<button type="submit" name="submit" value="submit" class="btn kanan bts-ats btn-primary">Publish</button>
  </div>
  <div class="col-md-4 whitebox">
      <div class=" form-group artikelkat">
              <div class="form-group bts-ats">
                <label class="col-sm-3 control-label">Kategori</label>
                  <div class="col-sm-8">
                    <select name="id_kategori" class="form-control">
                      <?php foreach ($getkategori as $kat): ?>
                        <option value="<?php echo $kat->id_kategori; ?>"><?php echo $kat->judul_kategori; ?></option>
                      <?php endforeach ?>
                    </select>
                  </div>
                <div class="sambungfloat"></div>
              </div>
              <div class="form-group bts-ats">
                <label class="col-sm-3 control-label">Tanggal</label>
                  <div class="col-sm-8">
                    <input type="date" name="tgl_artikel" class="form-control" value="<?php echo date('Y-m-d'); ?>">
                  </div>
                <div class="sambungfloat"></div>
              </div>
              <div class="form-group bts-ats">
                <label class="col-sm-3 control-label">Gambar</label>
                  <div class="col-sm-8">
                    <form>
                      <img id="preview" class="imgbox" src="<?php echo base_url('assets/img/artikel/default.jpg') ?>" alt="preview gambar">
                      <input id="filedata" type="file" name="photo" accept="image/*" />
                    </form>
                  </div>
                <div class="sambungfloat"></div>
              </div>

      </div>
  </div>
</form>

<script>
  function readURL(input) {
    if (input.files && input.files[0]) {
      var reader = new FileReader();
      reader.onload = function (e) {
        $('#preview').attr('src', e.target.result);
      }
      reader.readAsDataURL(input.files[0]);
    }
  }
  $('#filedata').change(function(){
    readURL(this);
  });
</script>
