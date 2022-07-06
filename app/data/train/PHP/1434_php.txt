<div id="print_area" style="width:200px; font-size:12px;">
<?php 
include('dbcon_s.php'); 
$date=date('Y-m-d', $time_now);
if($_POST['h_status']=='off')
{
	$query=mysql_query("select count(*) as num_order, sum(person) as total_person, sum(order_total) as total_order, sum(discount) as total_discount, sum(ser_charge) as total_s_charge, sum(vat_total) as total_vat, sum(cash) as total_cash, date from order_list where status='false' AND date='$date' AND terminal='".$_POST['terminal']."'");	
}
else
{
	$query=mysql_query("select count(*) as num_order, sum(person) as total_person, sum(order_total) as total_order, sum(discount) as total_discount, sum(ser_charge) as total_s_charge, sum(vat_total) as total_vat, sum(cash) as total_cash, date from order_list where status='false' AND date='$date' AND terminal='".$_POST['terminal']."'");	
}
$row=mysql_fetch_array($query);
?>
	<h2 style="text-align:center; font-family:Forte; margin:0px; padding:0px;">La Bamba</h2>
            <p style="text-align:center; margin:0px; font-size:12px;">
            	House # 54, Road # 20,<br />
                Sector # 03, Rabindra Sarani<br />
                Uttara, Dhaka-1230<br />
                Phone : 01759783896-7<br />
		Vat Reg No: 5111110711
            </p>
            <p style="text-align:center; margin:0px; font-size:12px;">
            	Day Report (<?php echo $_POST['terminal']; ?>)
            </p>
            Report Date : <?php echo $row['date']; ?><br />
            Print Date : <?php echo $date; ?><br />
            <table cellspacing="0" style="width:100%; font-size:12px;">
                <tr>
                    <th style="text-align:left">Number of order:</th>
                    <th style="text-align:left;"><?php echo $row['num_order']; ?></th>
                </tr>
                <tr>
                    <th style="text-align:left">Total Person:</th>
                    <th style="text-align:left;"><?php echo $row['total_person']; ?></th>
                </tr>
                <tr>
                    <th style="text-align:left">Total Order:</th>
                    <th style="text-align:left;"><?php echo $row['total_order']; ?></th>
                </tr>
                <tr>
                    <th style="text-align:left">Total Discount:</th>
                    <th style="text-align:left;"><?php echo $row['total_discount']; ?></th>
                </tr>
                <tr>
                    <th style="text-align:left">Total Service Charge:</th>
                    <th style="text-align:left;"><?php echo $row['total_s_charge']; ?></th>
                </tr>
                <tr>
                    <th style="text-align:left">Total Vat:</th>
                    <th style="text-align:left;"><?php echo $row['total_vat']; ?></th>
                </tr>
                <tr>
                    <th style="text-align:left">Total Cash:</th>
                    <th style="text-align:left;"><?php echo $row['total_cash']; ?></th>
                </tr>
			</table>
            <br />
            <div style="border-bottom:1px dotted #000;"></div>
            <br />
            Day report (<?php echo $_POST['terminal']; ?>) printed by : <?php echo $_POST['user']; ?>
</div>            
