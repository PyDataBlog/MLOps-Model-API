<?php
App::uses('AppController', 'Controller');
/**
 * GuestsUsers Controller
 *
 * @property GuestsUser $GuestsUser
 * @property PaginatorComponent $Paginator
 */
class GuestsUsersController extends AppController {

/**
 * Components
 *
 * @var array
 */
	public $components = array('Paginator');

/**
 * index method
 *
 * @return void
 */
	public function index() {
		$this->GuestsUser->recursive = 0;
		$this->set('guestsUsers', $this->Paginator->paginate());
	}

/**
 * view method
 *
 * @throws NotFoundException
 * @param string $id
 * @return void
 */
	public function view($id = null) {
		if (!$this->GuestsUser->exists($id)) {
			throw new NotFoundException(__('Invalid guests user'));
		}
		$options = array('conditions' => array('GuestsUser.' . $this->GuestsUser->primaryKey => $id));
		$this->set('guestsUser', $this->GuestsUser->find('first', $options));
	}

/**
 * add method
 *
 * @return void
 */
	public function add() {
		if ($this->request->is('post')) {
			$this->GuestsUser->create();
			if ($this->GuestsUser->save($this->request->data)) {
				$this->Session->setFlash(__('The guests user has been saved.'));
				return $this->redirect(array('action' => 'index'));
			} else {
				$this->Session->setFlash(__('The guests user could not be saved. Please, try again.'));
			}
		}
	}

/**
 * edit method
 *
 * @throws NotFoundException
 * @param string $id
 * @return void
 */
	public function edit($id = null) {
		if (!$this->GuestsUser->exists($id)) {
			throw new NotFoundException(__('Invalid guests user'));
		}
		if ($this->request->is(array('post', 'put'))) {
			if ($this->GuestsUser->save($this->request->data)) {
				$this->Session->setFlash(__('The guests user has been saved.'));
				return $this->redirect(array('action' => 'index'));
			} else {
				$this->Session->setFlash(__('The guests user could not be saved. Please, try again.'));
			}
		} else {
			$options = array('conditions' => array('GuestsUser.' . $this->GuestsUser->primaryKey => $id));
			$this->request->data = $this->GuestsUser->find('first', $options);
		}
	}

/**
 * delete method
 *
 * @throws NotFoundException
 * @param string $id
 * @return void
 */
	public function delete($id = null) {
		$this->GuestsUser->id = $id;
		if (!$this->GuestsUser->exists()) {
			throw new NotFoundException(__('Invalid guests user'));
		}
		$this->request->onlyAllow('post', 'delete');
		if ($this->GuestsUser->delete()) {
			$this->Session->setFlash(__('The guests user has been deleted.'));
		} else {
			$this->Session->setFlash(__('The guests user could not be deleted. Please, try again.'));
		}
		return $this->redirect(array('action' => 'index'));
	}}
