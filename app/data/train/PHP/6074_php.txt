<?php
/**
 * Created by PhpStorm.
 * User: phpNT
 * Date: 10.03.2016
 * Time: 17:33
 */
namespace common\widgets\ServiceLeftBlock;

use Yii;
use yii\base\Widget;

class ServiceLeftBlock extends Widget
{

    public function init() {
        parent::init();
    }

    public function run()
    {
        return $this->render(
            'view',
            [
                'widget' => $this,
            ]);
    }

}
