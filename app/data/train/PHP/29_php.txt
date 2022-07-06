
<div class="col-xs-7">
  <div class="box">
    <div class="box-header">
      <h3 class="box-title">Order Table</h3>
      <div class="box-tools">
        <div class="input-group input-group-sm" style="width: 150px;">
          <input type="search" class="light-table-filter form-control pull-right" data-table="order-table" placeholder="Search">
          <div class="input-group-btn">
            <button type="submit" class="btn btn-default"><i class="fa fa-search"></i></button>
          </div>
        </div>
      </div>
    </div>
    <!-- /.box-header -->
    <div class="box-body table-responsive no-padding">
     <table class="table table-hover order-table">
      <thead>
       <tr>
        <th>Product</th>
        <th>Customer</th>
        <th>Date</th>
        <th>Status</th>
        <th>Price</th>
        <th>Phone</th>
        <th></th>
      </tr>
    </thead>
    <tbody>

    <?php foreach ($order as $item): 
    if($item->status == 0){
      ?> 
      <tr>
        <td><?php echo $item->product; ?></td>
        <td><?php echo $item->username; ?></td>
        <td><?php echo $item->datereceive; ?></td>
        <td><?php if($item->status == 0){
          echo "Watting access...";
        }
        else{
          echo "Ok";
        }
        ?></td>
        <td><?php echo $item->price ?></td>
        <td><?php echo $item->phone ?></td>
        
        <td>
          <?php echo Html::anchor('admin/order/'.$item->id, 'Click to Complete', array('onclick' => "return confirm('Are you sure?')")); ?>
        </td>
      </tr>
      <?php 
    }
    endforeach; ?>
  </tbody>
</table>
</div>
<!-- /.box-body -->
</div>
<!-- /.box -->
</div>
<div class="col-xs-1"></div>
<div class="col-xs-4">
  <div class="box">
    <div class="box-header">
      <h3 class="box-title">Order Sussess</h3>
    </div>
    <!-- /.box-header -->
    <div class="box-body table-responsive no-padding">
      <?php if ($order): ?>
        <table class="table table-hover">
          <tbody><tr>
            <th>Product</th>
            <th>Date</th>
            <th>Status</th>
            <th>Customer</th>
          </tr>
          <?php foreach ($order as $item): 
          if($item->status == 1){
            ?>
            <tr>
              <td><?php echo $item->product; ?></td>
              <td><?php echo $item->datereceive; ?></td>
              <td><?php echo $item->username; ?></td>
              <td><?php if($item->status == 0){
                echo "Watting access...";
              }
              else{
                echo "Ok";
              }
              ?></td>
            </tr>
            <?php
          }
          ?>    
        <?php endforeach; ?>
      </tbody>
    </table>
  <?php endif; ?>

</div>
<!-- /.box-body -->
</div>
</div>