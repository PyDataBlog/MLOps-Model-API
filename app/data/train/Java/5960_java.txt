package br.com.fuelclub.entity;

import java.util.List;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.Table;

@Entity
@Table (name = "Dias_da_Semana")
public class Dias_Da_Semana {
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long diasDaSemana_id;
	private String diasDaSemana_descricao;
	
	@ManyToMany
	private List<PostoCombustivel> postos;

	public Long getDiasDaSemana_id() {
		return diasDaSemana_id;
	}

	public void setDiasDaSemana_id(Long diasDaSemana_id) {
		this.diasDaSemana_id = diasDaSemana_id;
	}

	public String getDiasDaSemana_descricao() {
		return diasDaSemana_descricao;
	}

	public void setDiasDaSemana_descricao(String diasDaSemana_descricao) {
		this.diasDaSemana_descricao = diasDaSemana_descricao;
	}

	public List<PostoCombustivel> getPostos() {
		return postos;
	}

	public void setPostos(List<PostoCombustivel> postos) {
		this.postos = postos;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((diasDaSemana_descricao == null) ? 0 : diasDaSemana_descricao.hashCode());
		result = prime * result + ((diasDaSemana_id == null) ? 0 : diasDaSemana_id.hashCode());
		result = prime * result + ((postos == null) ? 0 : postos.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Dias_Da_Semana other = (Dias_Da_Semana) obj;
		if (diasDaSemana_descricao == null) {
			if (other.diasDaSemana_descricao != null)
				return false;
		} else if (!diasDaSemana_descricao.equals(other.diasDaSemana_descricao))
			return false;
		if (diasDaSemana_id == null) {
			if (other.diasDaSemana_id != null)
				return false;
		} else if (!diasDaSemana_id.equals(other.diasDaSemana_id))
			return false;
		if (postos == null) {
			if (other.postos != null)
				return false;
		} else if (!postos.equals(other.postos))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "Dias_Da_Semana [diasDaSemana_descricao=" + diasDaSemana_descricao + "]";
	}

	public Dias_Da_Semana(Long diasDaSemana_id, String diasDaSemana_descricao, List<PostoCombustivel> postos) {
		super();
		this.diasDaSemana_id = diasDaSemana_id;
		this.diasDaSemana_descricao = diasDaSemana_descricao;
		this.postos = postos;
	}

	public Dias_Da_Semana() {
		super();
		// TODO Auto-generated constructor stub
	}

	
}
