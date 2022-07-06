package ExercicioPOO;

public class TestaAgenda {

    public static void main(String[] args) {
	Agenda agenda = new Agenda(2);
	Pessoa p1 = new Pessoa();
	p1.setNome("Emiliano");
	p1.setIdade(43);
	p1.setAltura(1.82);
	
	Pessoa p2 = new Pessoa();
	p2.setNome("Carvalho");
	p2.setIdade(34);
	p2.setAltura(1.82);

	agenda.armazenaPessoa(p1.getNome(), p1.getIdade(), p1.getAltura());
	agenda.armazenaPessoa(p2.getNome(), p2.getIdade(), p2.getAltura());
	
	agenda.imprimePessoa(0);
	agenda.imprimeAgenda();
    }

}
