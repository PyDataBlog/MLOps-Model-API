<?php

use yii\helpers\Html;
use yii\widgets\ActiveForm;

/* @var $this yii\web\View */
/* @var $model frontend\controllers\VnStatSearch */
/* @var $form yii\widgets\ActiveForm */
?>

<div class="vn-stat-search">

    <?php $form = ActiveForm::begin([
        'action' => ['index'],
        'method' => 'get',
    ]); ?>

    <?= $form->field($model, 'vn') ?>

    <?= $form->field($model, 'hn') ?>

    <?= $form->field($model, 'pdx') ?>

    <?= $form->field($model, 'gr504') ?>

    <?= $form->field($model, 'lastvisit') ?>

    <?php // echo $form->field($model, 'accident_code') ?>

    <?php // echo $form->field($model, 'dx_doctor') ?>

    <?php // echo $form->field($model, 'dx0') ?>

    <?php // echo $form->field($model, 'dx1') ?>

    <?php // echo $form->field($model, 'dx2') ?>

    <?php // echo $form->field($model, 'dx3') ?>

    <?php // echo $form->field($model, 'dx4') ?>

    <?php // echo $form->field($model, 'dx5') ?>

    <?php // echo $form->field($model, 'sex') ?>

    <?php // echo $form->field($model, 'age_y') ?>

    <?php // echo $form->field($model, 'age_m') ?>

    <?php // echo $form->field($model, 'age_d') ?>

    <?php // echo $form->field($model, 'aid') ?>

    <?php // echo $form->field($model, 'moopart') ?>

    <?php // echo $form->field($model, 'count_in_month') ?>

    <?php // echo $form->field($model, 'count_in_year') ?>

    <?php // echo $form->field($model, 'pttype') ?>

    <?php // echo $form->field($model, 'income') ?>

    <?php // echo $form->field($model, 'paid_money') ?>

    <?php // echo $form->field($model, 'remain_money') ?>

    <?php // echo $form->field($model, 'uc_money') ?>

    <?php // echo $form->field($model, 'item_money') ?>

    <?php // echo $form->field($model, 'dba') ?>

    <?php // echo $form->field($model, 'spclty') ?>

    <?php // echo $form->field($model, 'vstdate') ?>

    <?php // echo $form->field($model, 'op0') ?>

    <?php // echo $form->field($model, 'op1') ?>

    <?php // echo $form->field($model, 'op2') ?>

    <?php // echo $form->field($model, 'op3') ?>

    <?php // echo $form->field($model, 'op4') ?>

    <?php // echo $form->field($model, 'op5') ?>

    <?php // echo $form->field($model, 'rcp_no') ?>

    <?php // echo $form->field($model, 'print_count') ?>

    <?php // echo $form->field($model, 'print_done') ?>

    <?php // echo $form->field($model, 'pttype_in_region') ?>

    <?php // echo $form->field($model, 'pttype_in_chwpart') ?>

    <?php // echo $form->field($model, 'pcode') ?>

    <?php // echo $form->field($model, 'hcode') ?>

    <?php // echo $form->field($model, 'inc01') ?>

    <?php // echo $form->field($model, 'inc02') ?>

    <?php // echo $form->field($model, 'inc03') ?>

    <?php // echo $form->field($model, 'inc04') ?>

    <?php // echo $form->field($model, 'inc05') ?>

    <?php // echo $form->field($model, 'inc06') ?>

    <?php // echo $form->field($model, 'inc07') ?>

    <?php // echo $form->field($model, 'inc08') ?>

    <?php // echo $form->field($model, 'inc09') ?>

    <?php // echo $form->field($model, 'inc10') ?>

    <?php // echo $form->field($model, 'inc11') ?>

    <?php // echo $form->field($model, 'inc12') ?>

    <?php // echo $form->field($model, 'inc13') ?>

    <?php // echo $form->field($model, 'inc14') ?>

    <?php // echo $form->field($model, 'inc15') ?>

    <?php // echo $form->field($model, 'inc16') ?>

    <?php // echo $form->field($model, 'hospmain') ?>

    <?php // echo $form->field($model, 'hospsub') ?>

    <?php // echo $form->field($model, 'pttypeno') ?>

    <?php // echo $form->field($model, 'pttype_expire') ?>

    <?php // echo $form->field($model, 'cid') ?>

    <?php // echo $form->field($model, 'main_pdx') ?>

    <?php // echo $form->field($model, 'inc17') ?>

    <?php // echo $form->field($model, 'inc_drug') ?>

    <?php // echo $form->field($model, 'inc_nondrug') ?>

    <?php // echo $form->field($model, 'pt_subtype') ?>

    <?php // echo $form->field($model, 'rcpno_list') ?>

    <?php // echo $form->field($model, 'ym') ?>

    <?php // echo $form->field($model, 'node_id') ?>

    <?php // echo $form->field($model, 'ill_visit') ?>

    <?php // echo $form->field($model, 'count_in_day') ?>

    <?php // echo $form->field($model, 'pttype_begin') ?>

    <?php // echo $form->field($model, 'lastvisit_hour') ?>

    <?php // echo $form->field($model, 'rcpt_money') ?>

    <?php // echo $form->field($model, 'discount_money') ?>

    <?php // echo $form->field($model, 'old_diagnosis') ?>

    <?php // echo $form->field($model, 'debt_id_list') ?>

    <?php // echo $form->field($model, 'vn_guid') ?>

    <?php // echo $form->field($model, 'lastvisit_vn') ?>

    <?php // echo $form->field($model, 'hos_guid') ?>

    <div class="form-group">
        <?= Html::submitButton('Search', ['class' => 'btn btn-primary']) ?>
        <?= Html::resetButton('Reset', ['class' => 'btn btn-default']) ?>
    </div>

    <?php ActiveForm::end(); ?>

</div>
