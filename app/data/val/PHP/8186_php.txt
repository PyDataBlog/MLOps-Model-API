<?php
class sanpham
{
	function select_caocap()
	{
		$arr = array();
		$query = "SELECT * FROM sanpham ORDER BY gia  DESC LIMIT 3";
		$result = mysql_query($query) or die(mysql_error());
		while($row = mysql_fetch_assoc($result))
		{
			$arr[] = $row;
		}
		return $arr;
	}
	
	function select_capnhat()
	{
		$arr = array();
		$query = "SELECT * FROM sanpham LIMIT 4";
		$result = mysql_query($query) or die(mysql_error());
		while($row = mysql_fetch_assoc($result))
		{
			$arr[] = $row;
		}
		return $arr;
	}
	
	function read($id)
	{
	    $query = "SELECT * FROM sanpham WHERE id= $id";
		$result = mysql_query($query) or die(mysql_error());
		$row = mysql_fetch_assoc($result);
		return $row;
	}
	function xl()
	{
		   $query = "SELECT * FROM sanpham";
           $result = mysql_query($query) or die(mysql_error());
           while($row = mysql_fetch_array($result))
    { 		   
           if(isset($_SESSION[".$row[id]."])==true)
		   {   
		   $p = $p." ".$row['id'];
		   }
    }
	
        return $p;
		
	}
	
	function ngay() {
    date_default_timezone_set('Asia/Ho_Chi_Minh');
    return date('d/m/Y');
}
	
}
?>