<div class="modal fade " id="modal_add" tabindex="-1" role="dialog" aria-labelledby="add_gambar">
    <div class="modal-dialog modal-lg" role="document">
        <div class="modal-content">
            <div class="modal-header">
                <a type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></a>
                <h4 class="modal-title" >Add Album Otsukaresama</span></h4>
            </div>
            
            <form method="post" role="form" action="<?php echo base_url('index.php/admin/otsukaresama_konten_add') ?>" enctype="multipart/form-data">
                <div class="modal-body">
                    
                    <input type="hidden" name="id_otsukaresama" value="<?php echo $id_otsukaresama; ?>">


                    <div class="form-group">
                        <label for="exampleInputFile">
                            Gambar
                        </label>
                        <input name="url" type="file" id="exampleInputFile">
                    </div>
                        

                </div>
                <div class="modal-footer">
                    <a type="button" class="btn btn-default" data-dismiss="modal">Close</a>
                    <button type="submit" class="btn btn-success">Tambah</button>
                </div>
            </form>
            
        </div>
    </div>
</div>