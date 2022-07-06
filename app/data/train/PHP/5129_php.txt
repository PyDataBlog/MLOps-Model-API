<?php
/*
 * This file is part of the BootstrapPHP package.
 *
 * (c) Milos Danilov <milosdanilov@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace BootstrapPHP\Components\Dropdown;

/**
 * Class DropdownItem
 *
 * @package BootstrapPHP\Components\Dropdown
 * @author Milos Danilov <milosdanilov@gmail.com>
 */
class DropdownItem
{
    protected $content;
    protected $disabled = false;

    public function __construct($content, $disabled = false)
    {
        $this->content = $content;
        $this->disabled = $disabled;
    }

    protected function getDisabled()
    {
        return $this->disabled ? "class='disabled'" : '';
    }

    public function __toString()
    {
        return "<li {$this->getDisabled()}>{$this->content}</li>";
    }
}