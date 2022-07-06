<div class="pull-right">
	<a href="<?php echo site_url('customer_loan_payment/add'); ?>" class="btn btn-success">Add</a> 
</div>

<table class="table table-striped table-bordered">
    <tr>
		<th>Payment Date</th>
		<th>Receipt Id</th>
		<th>Late Pay Charges</th>
		<th>Lod Charges</th>
		<th>Site Vist Charges</th>
		<th>Other Charges</th>
		<th>Wd Amount</th>
		<th>Recevied Amount</th>
		<th>Card Balance</th>
		<th>Created By</th>
		<th>Modified By</th>
		<th>Created Date</th>
		<th>Modified Date</th>
		<th>Payment Id</th>
		<th>Payment Method</th>
		<th>Wd By</th>
		<th>Actions</th>
    </tr>
	<?php foreach($customer_loan_payments as $c){ ?>
    <tr>
		<td><?php echo $c['payment_date']; ?></td>
		<td><?php echo $c['receipt_id']; ?></td>
		<td><?php echo $c['late_pay_charges']; ?></td>
		<td><?php echo $c['lod_charges']; ?></td>
		<td><?php echo $c['site_vist_charges']; ?></td>
		<td><?php echo $c['other_charges']; ?></td>
		<td><?php echo $c['wd_amount']; ?></td>
		<td><?php echo $c['recevied_amount']; ?></td>
		<td><?php echo $c['card_balance']; ?></td>
		<td><?php echo $c['created_by']; ?></td>
		<td><?php echo $c['modified_by']; ?></td>
		<td><?php echo $c['created_date']; ?></td>
		<td><?php echo $c['modified_date']; ?></td>
		<td><?php echo $c['payment_id']; ?></td>
		<td><?php echo $c['payment_method']; ?></td>
		<td><?php echo $c['wd_by']; ?></td>
		<td>
            <a href="<?php echo site_url('customer_loan_payment/edit/'.$c['payment_id']); ?>" class="btn btn-info">Edit</a> 
            <a href="<?php echo site_url('customer_loan_payment/remove/'.$c['payment_id']); ?>" class="btn btn-danger">Delete</a>
        </td>
    </tr>
	<?php } ?>
</table>