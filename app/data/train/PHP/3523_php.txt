<?php

if ($_SERVER['REQUEST_METHOD'] == 'POST') {
    extract($_POST);
    extract($_GET);

    $pdo = conectar();

    $query = selectPinEvento();
    $stmt = $pdo->prepare($query);
    $stmt->bindParam(":id", $id);
    $stmt->bindParam(":pin", $pin);
    $stmt->execute();

    if ($stmt->rowCount() == 1) {
        if ($a == 'ed') {
            $pdo = null;
            $stmt = null;
            header("Location: ./?p=ev&id=$id");
            exit;
        } elseif ($a == 'ex') {
            $pdo = null;
            $stmt = null;
            header("Location: ./?p=exm&id=$id");
            exit;
        }
    } else {
        $pdo = null;
        $stmt = null;
        header("Location: ./?p=agv&m=pi");
        exit;
    }
}
?>
