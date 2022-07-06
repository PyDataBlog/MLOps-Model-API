<?php

namespace common\models;

use common\modules\i18n\Module;

/**
 * This is the model class for table "magazine_item".
 *
 * @property integer $id
 * @property integer $magazine_id
 * @property string $image
 * @property integer $sort
 *
 * @property Magazine $magazine
 */
class MagazineItem extends Bean
{

    /**
     * Variable for file storing while data saving
     * @var mixed
     */
    public $file;

    /**
     * @inheritdoc
     */
    public static function tableName()
    {
        return 'magazine_item';
    }

    /**
     * @inheritdoc
     */
    public function rules()
    {
        return [
            [['magazine_id', 'image'], 'required'],
            [['magazine_id', 'sort'], 'integer'],
            [['image'], 'string', 'max' => 255],
            [['magazine_id'], 'exist', 'skipOnError' => true, 'targetClass' => Magazine::className(), 'targetAttribute' => ['magazine_id' => 'id']],
        ];
    }

    /**
     * @inheritdoc
     */
    public function attributeLabels()
    {
        return [
            'id'          => Module::t('Id'),
            'magazine_id' => Module::t('Magazine'),
            'image'       => Module::t('Image'),
        ];
    }

    /**
     * @return \yii\db\ActiveQuery
     */
    public function getMagazine()
    {
        return $this->hasOne(Magazine::className(), ['id' => 'magazine_id']);
    }
}
