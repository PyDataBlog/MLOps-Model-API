package org.jiserte.bioformats.readers.phylip;

import org.jiserte.bioformats.readers.faults.AlignmentReadingFault;

public class FirstBlockLinePhylipFault extends AlignmentReadingFault {

	public FirstBlockLinePhylipFault() {

		super();
		
		this.setMessage("Sequences in the first block of data must have a description of 10 characters and then the sequence.");
		
	}
	
	

}
