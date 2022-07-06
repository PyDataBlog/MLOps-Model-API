<?php

use yii\helpers\Html;
use yii\widgets\ActiveForm;

/* @var $this yii\web\View */
/* @var $model app\models\IzinmenutupjalanSearch */
/* @var $form yii\widgets\ActiveForm */
?>

<div class="izinmenutupjalan-search">

    <?php $form = ActiveForm::begin([
        'action' => ['index'],
        'method' => 'get',
    ]); ?>

    <?= $form->field($model, 'id_imj') ?>

    <?= $form->field($model, 'id_akun') ?>

    <?= $form->field($model, 'nomor_imj') ?>

    <?= $form->field($model, 'tglDikeluarkan_imj') ?>

    <?= $form->field($model, 'tglPengajuan_imj') ?>

    <?php // echo $form->field($model, 'kepada_imj') ?>

    <?php // echo $form->field($model, 'alamat_imj') ?>

    <?php // echo $form->field($model, 'namaAcara_imj') ?>

    <?php // echo $form->field($model, 'jalanDitutup_imj') ?>

    <?php // echo $form->field($model, 'penutupanJalanMaksimal_imj') ?>

    <?php // echo $form->field($model, 'tglAcara_imj') ?>

    <?php // echo $form->field($model, 'JamAcara_imj') ?>

    <?php // echo $form->field($model, 'tglAcaraSelesai_imj') ?>

    <?php // echo $form->field($model, 'jamAcaraSelesai_imj') ?>

    <?php // echo $form->field($model, 'tembusan_imj') ?>

    <?php // echo $form->field($model, 'keterangan_imj') ?>

    <div class="form-group">
        <?= Html::submitButton('Search', ['class' => 'btn btn-primary']) ?>
        <?= Html::resetButton('Reset', ['class' => 'btn btn-default']) ?>
    </div>

    <?php ActiveForm::end(); ?>

</div>
