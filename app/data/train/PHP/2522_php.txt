<?php
$id = $_GET['id'];
$link = mysqli_connect("localhost",
    "root",
    "lict@2",
    "crud");
$query = "select * from ict_skills WHERE id = $id";
$result = mysqli_query($link, $query);
$row = mysqli_fetch_assoc($result);
?>
<form action="update.php" method="post">
    <input type="hidden" name="id" value="<?php echo $row['id'];?>" />
    <br>
    <label>Experience Category:</label>
    <select name = "ecategory">
        <option <?php if ($row ['ecategory']== "computer enginering") echo 'selected'; ?> value="computer enginering">Computer Enginering</option>
        <option <?php if ($row ['ecategory']== "accounting") echo 'selected'; ?> value="accounting">Accounting</option>
        <option <?php if ($row ['ecategory']== "bank/non-bank") echo 'selected'; ?> value="bank/non-bank">Bank/non-bank</option>
        <option <?php if ($row ['ecategory']== "design/creative") echo 'selected'; ?> value="design/creative">Design/creative</option>

        </select>
    <br>
    <label>Skills:</label>
    <select name = "skills">
        <option <?php if ($row ['skills']== "programming") echo 'selected'; ?> value="programming">Programming</option>
        <option <?php if ($row ['skills']== "database") echo 'selected'; ?> value="database">Database</option>
        <option <?php if ($row ['skills']== "xpath/xquery/xlink/xpointer") echo 'selected'; ?> value="xpath/xquery/xlink/xpointer">Xpath/Xquery/Xlink/Xpointer</option>

    </select>
    <br>

    <label>Skill Description:</label>
    <input type="text" name="sdescription" value="<?php echo $row['sdescription'];?>" />
    <br>
    <label> Extracurricular Activities:</label>
    <input type="text" name="eactivities" value="<?php echo $row['eactivities'];?>" />

    <br>
    <button type="submit">Save</button>
    <button type="reset">Cancel</button>

</form>
</html>

<a href="list.php">Go to list</a>