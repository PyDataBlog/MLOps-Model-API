<?php
$query = "select * from `joinjoin_true` where joinjoin_true.species_chinesename = ".'"'.$animalChineseName.'"'." limit $begin,$limit;";
//连接数据库
$conn = @mysqli_connect("127.0.0.1","guest","guest",$db);
if(mysqli_connect_errno($conn))
{
	die("无法连接数据库，请联系服务器管理员");
}else{
	mysqli_set_charset($conn,"utf8");
	$query = mysqli_real_escape_string($conn,$query);
	$query = str_replace("\\","",$query);
	//echo $query."\n";
}

$result = mysqli_query($conn,$query);


function printSQLresultAsTable(){
	global $conn;
	global $result;
//输出表头
echo <<<tableHead
<table class="table">
<caption>查询结果<caption>
<thead>
tableHead;
echo '<tr>';
$fetch_fields = mysqli_fetch_fields($result);
foreach($fetch_fields as $column)
{
	echo "<th class=\"warning\">$column->name</th>";
}
echo "</tr>";
echo '</thead>';
//逐行输出数据表
echo "<tbody>";
	while($row=mysqli_fetch_array($result,MYSQLI_NUM))
	{
		if($row[5]=='TRUE')
		{
				echo '<tr class="success">';
		}else{
			echo '<tr class="danger">';
		}
		foreach($row as $value)
		{
			echo "<td>$value</td>";
		}
		echo "</tr>";
	}
}
echo "</tbody>";
mysqli_close($conn);

?>