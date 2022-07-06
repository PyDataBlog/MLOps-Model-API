<?php

use yii\helpers\Html;
use yii\grid\GridView;
use yii\widgets\Pjax;



$this->title = Yii::t('app', 'Visitas');
$this->params['breadcrumbs'][] = $this->title;
?>
<div class="visita-index">

    <h1><?= Html::encode($this->title) ?></h1>
    <?php Pjax::begin(); ?>
    <?php // echo $this->render('_search', ['model' => $searchModel]); ?>

    <p>
        <?= Html::a(Yii::t('app', 'Create Visita'), ['create'], ['class' => 'btn btn-success']) ?>
    </p>

    <?= GridView::widget([
        'dataProvider' => $dataProvider,
        'filterModel' => $searchModel,
        'columns' => [
            //['class' => 'yii\grid\SerialColumn'],

            //'id',
            
            [
                'attribute'=>'id_entidad',
                'value'=> function($model){
                    return \backend\models\Entidad::getNameEntity($model->id_entidad);
                }
            ],
            //'id_establecimiento',
            //'fecha_evento',
            //'causa_visita',
            // 'estado_visita',
            'concepto_visita',
            // 'tipo_identificacion_funcionario',
            // 'numero_identidad_funcionario',
            // 'apellidos_funcionario',
            // 'nombres_funcionario',
            // 'tipo_identidad_atiende',
            // 'numero_identidad_atiende',
            // 'apellidos_atiende',
            // 'nombres_atiende',
            // 'observacion_visita',

            [
                'class' => 'yii\grid\ActionColumn',
                'template'=>''
                
            ],
        ],
    ]); ?>
    <?php Pjax::end(); ?>
</div>
