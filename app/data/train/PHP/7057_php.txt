<?php
/**
 * User: potworny
 * Date: 17.10.17
 * Time: 15:09
 */

namespace AdapterFacadeBundle\Devices;


class Screen
{
    /**
     * @var bool
     */
    private $down = false;

    public function down()
    {
        $this->down = true;
    }

    public function up()
    {
        $this->down = false;
    }
}