package tsp;
public class Ciudad {
	
	private String nombre;
	private int id;
	private CiudadesDistancias[] distanciasC = new CiudadesDistancias[22];
	private boolean visitado = false;
	private boolean inicial = false;
	
	Ciudad(int idd, String nom){
		nombre = nom;
		id = idd;
	}
	
	public String getNombre() {
		return nombre;
	}
	
	public void setNombre(String nombre) {
		this.nombre = nombre;
	}
	
	public int getId() {
		return id;
	}
	
	public void setId(int id) {
		this.id = id;
	}
	
	public CiudadesDistancias distanciasACiudades(int i) {
		return distanciasC[i];
	}
	
	public CiudadesDistancias[] getDistancias() {
		return distanciasC;
	}
	
	public void setDistancias(CiudadesDistancias[] distancias) {
		this.distanciasC = distancias;
	}
	
	public boolean getVisitado() {
		return visitado;
	}
	
	public void setVisitado(){
		this.visitado = true;
	}
	
	public void setInicial(){
		this.inicial = true;
	}
	
	public boolean getInicial()	{
		return inicial;
	}
	
	public int getDistancia(String cd){
		int distancia = 0;
		for (int i=0;i<22;i++){
			if(distanciasC[i].getCiudadDestino().equals(cd)){
				distancia=distanciasC[i].getDistancia();
			}		
		}
		return distancia;
	}
}
	
