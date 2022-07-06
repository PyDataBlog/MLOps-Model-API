<?php

namespace backend\models;

use Yii;

/**
 * This is the model class for table "customer".
 *
 * @property string $id
 * @property string $username
 * @property string $password
 * @property string $email
 * @property string $facebook_id
 * @property string $id_profile
 */
class Customer extends \yii\db\ActiveRecord
{
    /**
     * @inheritdoc
     */
    public static function tableName()
    {
        return 'customer';
    }

    /**
     * @inheritdoc
     */
    public function rules()
    {
        return [
            [['id_profile'], 'integer'],
            [['username', 'password', 'email', 'facebook_id'], 'string', 'max' => 255],
            
            //field2 standar untuk semua model
            ['row_status','default','value'=>1,'when' => function($model){ return $model->isNewRecord;}],            
            ['created_date','default','value'=> function ($model, $attribute) {return date('Y-m-d H:i:s');},'when' => function($model){ return $model->isNewRecord;}],
            ['created_by','default','value'=>\Yii::$app->user->identity->id,'when' => function($model){ return $model->isNewRecord;}],
            ['modified_date','default','value'=> function ($model, $attribute) {return date('Y-m-d H:i:s');},'when' => function($model){ return !$model->isNewRecord;}],
            ['modified_by','default','value'=>\Yii::$app->user->identity->id,'when' => function($model){ return !$model->isNewRecord;}],
        ];
    }

    /**
     * @inheritdoc
     */
    public function attributeLabels()
    {
        return [
            //'id' => Yii::t('app', 'ID'),
            'username' => Yii::t('app', 'Name'),
            //'password' => Yii::t('app', 'Password'),
            'email' => Yii::t('app', 'Email'),
            //'facebook_id' => Yii::t('app', 'Facebook ID'),
            'id_profile' => Yii::t('app', 'Radius'),
        ];
    }
}
