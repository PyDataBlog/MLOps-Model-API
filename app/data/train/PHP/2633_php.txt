<?php

/*
 * This file is part of the Topycs package.
 *
 * (c) Daniel Ribeiro <drgomesp@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
 
namespace Topycs\Discussions\Builder;

/**
 * Defines a fluent builder for threads.
 *
 * @author Daniel Ribeiro <drgomesp@gmail.com>
 * @package Topycs\Discussions\Builder
 */
interface ThreadBuilderInterface
{
    /**
     * Builds a thread.
     * 
     * @param string $title
     * @param string $content
     * @return \Topycs\Discussions\Entity\ThreadInterface
     */
    public function buildThread($title, $content);
}
