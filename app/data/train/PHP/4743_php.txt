<?php

    switch ($page){
        case 'psd-files':
            $subtitle = ' - PSD files included';
            break;
        case 'changelog':
            $subtitle = ' - Changelog';
            break;
        case 'assets':
            $subtitle = ' - Assets';
            break;
        default:
            $subtitle = '';
    }
    
?>
<!DOCTYPE HTML>
<html>
    <head>
        <title>My Heaven - Online Booking PSD Template<?php echo $subtitle; ?></title>

        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <meta name="description" content="My Heaven is a clean and complete booking PSD template ideal for hotels, guest houses, villas and more." />
        <meta name="keywords" content="availability, book, booking, calendar, holiday, hostel, hotel, rate, rent, reservation, room, schedule, travel, vacation, villa" />

        <?php include_once('../libraries/php/assets.php'); ?>
        <?php include_once('../libraries/php/google-analytics.php'); ?>
    </head>
    <body>
        <div id="wrapper">
            <div id="header">
                <h1>My Heaven - Online Booking PSD Template</h1>
            </div>  