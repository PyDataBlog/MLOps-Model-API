package br.ufpe.ines.decode.observer.control;

import java.io.File;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;

import br.ufpe.ines.decode.decode.CodingExperiment;
import br.ufpe.ines.decode.decode.DecodePackage;

public class ExperimentManager {

	protected static ExperimentManager singleton = new ExperimentManager();
	private Set<CodingExperiment> loadedExperiments2 = new HashSet<CodingExperiment>();
	private List<String> filePaths = new LinkedList<String>();
	static final Logger logger = Logger.getLogger(ExperimentManager.class);
	// private CountDownLatch latchAction = new CountDownLatch(1);

	public static ExperimentManager getInstance() {
		if (singleton == null)
			singleton = new ExperimentManager();
		return singleton;
	}

	protected ExperimentManager() {
	}

	public Set<CodingExperiment> getLoadedExperiments() {
		return loadedExperiments2;
	}

	public void loadDecodeModel(File filePath) {
		loadedExperiments2.add(load(filePath));
		filePaths.add(filePath.getAbsolutePath());
	}

	protected CodingExperiment load(File filePath) {
		ResourceSet resourceSet = new ResourceSetImpl();
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("decode", new XMIResourceFactoryImpl());
		DecodePackage.eINSTANCE.eClass();
		URI fileURI = URI.createFileURI(filePath.getAbsolutePath());
		Resource resource = resourceSet.getResource(fileURI, true);
		EObject myModelObject = resource.getContents().get(0);

		CodingExperiment myWeb = null;
		if (myModelObject instanceof CodingExperiment) {
			myWeb = (CodingExperiment) myModelObject;
		}
		return myWeb;
	}

	public List<String> getFiles() {
		return filePaths;
	}

	public CodingExperiment getLoadedExperiment(String experimentID) {
		return loadedExperiments2.stream()
						  .filter(e -> e.getElementId().equals(experimentID))
						  .findFirst().get();
		
	}
}
