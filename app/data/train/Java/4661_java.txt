package org.anddev.andengine.entity.layer.tiled.tmx;

import static junit.framework.Assert.assertEquals;

import java.io.File;

import org.junit.Test;

/**
 * @author Nicolas Gramlich
 * @since 21:21:51 - 28.07.2010
 */
public class TMXTiledMapArtifactFixerTest {
	// ===========================================================
	// Constants
	// ===========================================================

	// ===========================================================
	// Fields
	// ===========================================================

	// ===========================================================
	// Constructors
	// ===========================================================

	// ===========================================================
	// Getter & Setter
	// ===========================================================

	// ===========================================================
	// Methods for/from SuperClass/Interfaces
	// ===========================================================

	// ===========================================================
	// Methods
	// ===========================================================

	@Test
	public void testDetermineTileColumnCount() throws Exception {
		assertEquals(8, TMXTiledMapArtifactFixer.determineCount(265, 32, 1, 1));
		assertEquals(6, TMXTiledMapArtifactFixer.determineCount(199, 32, 1, 1));
	}
	
	@Test
	public void testGenerateOutputFile() throws Exception {
		assertEquals("C:\\Test\\fixed_bla.png", TMXTiledMapArtifactFixer.generateOutputFile(new File("C:\\Test\\bla.png")).toString());
	}

	// ===========================================================
	// Inner and Anonymous Classes
	// ===========================================================
}
