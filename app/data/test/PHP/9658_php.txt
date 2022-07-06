<?php
use PatrickRose\Repos\GigRepositoryInterface;

/**
 * Created by PhpStorm.
 * User: patrick
 * Date: 28/03/14
 * Time: 16:00
 */

class StaticPagesController extends BaseController {

    /**
     * @var GigRepositoryInterface The gig repository
     */
    private $repo;

    /**
     * @param GigRepositoryInterface $repository The repository we want to use
     */
    public function __construct(GigRepositoryInterface $repository) {
        $this->repo = $repository;
    }

    public function index(){
        return View::make('staticpages.index');
    }
}