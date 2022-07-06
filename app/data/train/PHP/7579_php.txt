<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateMatchesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        {
            Schema::create('matches', function (Blueprint $table) {
                $table->increments('id');
                $table->integer('m_number');
                $table->integer('m_homeid');
                $table->integer('m_guestid');
                $table->string('m_time');
                $table->date('m_date');
                $table->string('m_score');
                $table->string('m_ref_com'); //match referee comments
                $table->integer('m_homeg'); // match home team goals
                $table->integer('m_guestg');// match guest team goals
                $table->integer('field_id')->unsigned();
                $table->integer('tournament_id')->unsigned();
                $table->timestamps();
            });

            Schema::table('matches', function (Blueprint $table) {
                $table->foreign('field_id')->references('id')->on('fields')->onDelete('cascade');
            });

            Schema::table('matches', function (Blueprint $table) {
                $table->foreign('tournament_id')->references('id')->on('tournaments')->onDelete('cascade');
            });
        }
        //
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::drop('matches');
        //
    }
}
