<?php

    if($_GET["errormessage"]){
        $message = $_GET["errormessage"];
        echo "<p>$message</p>";
    }

    echo '<table style="width:100%">';
        echo '<tr>';
            echo '<td width="50%">'; include 'timebank-pages/login.php'; echo '</td>';
            echo '<td width="50%">'; include 'timebank-pages/signup.php'; echo '</td>';
        echo '</tr>';
    echo '</table>';
?>