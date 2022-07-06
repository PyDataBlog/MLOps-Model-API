package com.ufpb.questao9;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

public class RetangulosTest {
	
	Retangulos r;
	double base, altura;

	@Before
	public void setUp() throws Exception {
		base = 4;
		altura = 2;
		r = new Retangulos(base, altura);
		
	}

	@Test
	public void testCalculaArea() {
		assertEquals(8, r.calculaArea(), 0.1);
	}
	
	@Test
	public void testCalculaPerimetro(){
		assertEquals(12, r.calculaPerimetro(), 0.1);
	}

}
