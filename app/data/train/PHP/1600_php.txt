<?php
include_once 'dynamic_db_connection.php';
/**
 * This is the model class for table "THEME_MANAGEMENT".
 *
 * The followings are the available columns in table 'THEME_MANAGEMENT':
 * @property string $PID
 * @property string $THEME
 */
class Theme_Management extends CActiveRecord
{
	/**
	 * Returns the static model of the specified AR class.
	 * @param string $className active record class name.
	 * @return Theme_Management the static model class
	 */
	public static function model($className=__CLASS__)
	{
		return parent::model($className);
	}

	/**
	 * @return string the associated database table name
	 */
	public function tableName()
	{
		return 'THEME_MANAGEMENT';
	}

	/**
	 * @return array validation rules for model attributes.
	 */
	public function rules()
	{
		// NOTE: you should only define rules for those attributes that
		// will receive user inputs.
		return array(
			array('THEME', 'length', 'max'=>20),
			// The following rule is used by search().
			// Please remove those attributes that should not be searched.
			array('USER_ID,ID, THEME', 'safe', 'on'=>'search'),
		);
	}

	/**
	 * @return array relational rules.
	 */
	public function relations()
	{
		// NOTE: you may need to adjust the relation name and the related
		// class name for the relations automatically generated below.
		return array(
		);
	}

	/**
	 * @return array customized attribute labels (name=>label)
	 */
	public function attributeLabels()
	{
		return array(
			'USER_ID' => 'Userid',
			'THEME' => 'Theme',
		);
	}

	/**
	 * Retrieves a list of models based on the current search/filter conditions.
	 * @return CActiveDataProvider the data provider that can return the models based on the search/filter conditions.
	 */
	public function search()
	{
		// Warning: Please modify the following code to remove attributes that
		// should not be searched.

		$criteria=new CDbCriteria;

		$criteria->compare('USER_ID',$this->USER_ID,true);
		$criteria->compare('THEME',$this->THEME,true);

		return new CActiveDataProvider($this, array(
			'criteria'=>$criteria,
		));
	}
}