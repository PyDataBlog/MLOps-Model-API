<?php
/**
 * Intreface DAO
 *
 * @author: http://phpdao.com
 * @date: 2013-11-06 23:13
 */
interface AccesoDAO{

	/**
	 * Get Domain object by primry key
	 *
	 * @param String $id primary key
	 * @Return Acceso 
	 */
	public function load($id);

	/**
	 * Get all records from table
	 */
	public function queryAll();
	
	/**
	 * Get all records from table ordered by field
	 * @Param $orderColumn column name
	 */
	public function queryAllOrderBy($orderColumn);
	
	/**
 	 * Delete record from table
 	 * @param acceso primary key
 	 */
	public function delete($usuario);
	
	/**
 	 * Insert record to table
 	 *
 	 * @param Acceso acceso
 	 */
	public function insert($acceso);
	
	/**
 	 * Update record in table
 	 *
 	 * @param Acceso acceso
 	 */
	public function update($acceso);	

	/**
	 * Delete all rows
	 */
	public function clean();

	public function queryByContrasenia($value);

	public function queryByRol($value);


	public function deleteByContrasenia($value);

	public function deleteByRol($value);


}
?>