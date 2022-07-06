<?php

use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;

class CreateAbsensisTable extends Migration {

	/**
	 * Run the migrations.
	 *
	 * @return void
	 */
	public function up()
	{
		Schema::create('absensis', function(Blueprint $table)
		{
			$table->increments('id');
			$table->string(' kd_karyawan');
			$table->string('kd_absen');
			$table->date('tanggal');
			$table->string('cuti');
			$table->string('ijin');
			$table->string('sakit');
			$table->string ('alpha');
			$table->timestamps();
		});
	}


	/**
	 * Reverse the migrations.
	 *
	 * @return void
	 */
	public function down()
	{
		Schema::drop('absensis');
	}

}
