/**
 * @author jesus
 *
 */
public class Principal {

	public static void main(String[] args) {
		
		Persona alumno1 = new Alumno("03181199T","Jesus","Ortega Vilchez",true,true,8);
		Persona profesor1 = new Profesor("0156478M","Jose Carlos","Villar",true,false,1500.50);
		Persona profesorFP1 =  new ProfesorFP("02314566G","Javier","Olmedo Garcia",true,false,7);
		Persona profesorESO1 =  new ProfesorESO("02415874M","Jose Luis","Fernandez Pérez",true,false,"Informatica");
		
		System.out.println("DATOS: "+alumno1.toString());
		System.out.println("Es miembro? "+alumno1.esMiembro());
		System.out.println("DATOS: "+profesor1.toString());
		System.out.println("Es miembro? "+profesor1.esMiembro());
		System.out.println("DATOS: "+profesorFP1.toString());
		System.out.println("DATOS: "+profesorESO1.toString());

	}

}
