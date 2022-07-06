<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateHabitosTable extends Migration
{
    public function up()
    {
        Schema::create('habitos', function (Blueprint $table) {
            $table->increments('id');
            $table->integer('paciente_id')->unsigned();
            $table->foreign('paciente_id')->references('id')->on('pacientes')->onDelete('cascade');
            $table->string('sono_repouso'         )->nullable();
            $table->string('horas_sono'           )->nullable();
            $table->string('atividade_fisica'     )->nullable();
            $table->string('qtde_atividade_fisica')->nullable();
            $table->string('frutas_verduras'      )->nullable();
            $table->string('carne_vermelha'       )->nullable();
            $table->string('carne_branca'         )->nullable();
            $table->string('suco_habitos'         )->nullable();
            $table->string('agua_habitos'         )->nullable();
            $table->string('cha_habitos'          )->nullable();
            $table->text('outras_informacoes'     )->nullable();
            $table->timestamps();
        });
    }

    public function down()
    {
        Schema::dropIfExists('habitos');
    }
}