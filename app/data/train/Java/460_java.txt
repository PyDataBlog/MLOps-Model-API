/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.freteplanejado.entity;

/**
 *
 * @author 43596980895
 */
public class Frete {
    private int id;
    private String origem;
    private String destino;
    private float altura;
    private float largura;
    private float profundidade;
    private float peso;
    private String observacoes;

    public Frete() {
    }

    public Frete(String origem, String destino, float altura, float largura, float profundidade, float peso, String observacoes) {
        this.origem = origem;
        this.destino = destino;
        this.altura = altura;
        this.largura = largura;
        this.profundidade = profundidade;
        this.peso = peso;
        this.observacoes = observacoes;
    }
    
    

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getOrigem() {
        return origem;
    }

    public void setOrigem(String origem) {
        this.origem = origem;
    }

    public String getDestino() {
        return destino;
    }

    public void setDestino(String destino) {
        this.destino = destino;
    }

    public float getAltura() {
        return altura;
    }

    public void setAltura(float altura) {
        this.altura = altura;
    }

    public float getLargura() {
        return largura;
    }

    public void setLargura(float largura) {
        this.largura = largura;
    }

    public float getProfundidade() {
        return profundidade;
    }

    public void setProfundidade(float profundidade) {
        this.profundidade = profundidade;
    }

    public float getPeso() {
        return peso;
    }

    public void setPeso(float peso) {
        this.peso = peso;
    }

    public String getObservacoes() {
        return observacoes;
    }

    public void setObservacoes(String observacoes) {
        this.observacoes = observacoes;
    }
}
