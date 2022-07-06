
public class Rectangulo {
	public int Base;
	public int Altura;
//Ejercicio realizado con ayuda de esta pagina:http://diagramas-de-flujo.blogspot.com/2013/02/calcular-perimetro-rectangulo-Java.html
	//aqui llamamos las dos variables que utilizaremos.
	Rectangulo(int Base, int Altura)
	{
		this.Base = Base;
		this.Altura = Altura;
	}
//COmo pueden observar aqui se obtiene la base y se asigna el valor de la base.
	int getBase ()
	{
		return Base;
	}

	//aqui devolvemos ese valor
	void setBase (int Base)
	{
		this.Base = Base;
	}
//aqui de igual forma se obtiene la altura y se le asigna el valor
	int getAltura ()
	{
		return Altura;
	}
//aqui devuelve el valor de la altura 
	void setAltura (int Altura)
	{
		this.Altura = Altura;
	}
	//aqui con una formula matematica se obtiene el perimetro hacemos una suma y una multiplicacion
	int getPerimetro()
	{
		return 2*(Base+Altura);
	}
	//aqui solo se hace un calculo matematico como la multiplicacion
	int getArea()
	{
		return Base*Altura;
	}

	

}
