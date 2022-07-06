/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package de.hub.specificmodels.tests.testsourcemodel.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.impl.EPackageImpl;

import de.hub.specificmodels.tests.testsourcemodel.ClassWithListFeatures;
import de.hub.specificmodels.tests.testsourcemodel.ListFeatureElementClass1;
import de.hub.specificmodels.tests.testsourcemodel.ListFeatureElementClass2;
import de.hub.specificmodels.tests.testsourcemodel.ListFeatureElementClass3;
import de.hub.specificmodels.tests.testsourcemodel.RootClass;
import de.hub.specificmodels.tests.testsourcemodel.TestSourceModelFactory;
import de.hub.specificmodels.tests.testsourcemodel.TestSourceModelPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class TestSourceModelPackageImpl extends EPackageImpl implements TestSourceModelPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass rootClassEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass classWithListFeaturesEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass listFeatureElementClass1EClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass listFeatureElementClass2EClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass listFeatureElementClass3EClass = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see de.hub.specificmodels.tests.testsourcemodel.TestSourceModelPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private TestSourceModelPackageImpl() {
		super(eNS_URI, TestSourceModelFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 * 
	 * <p>This method is used to initialize {@link TestSourceModelPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static TestSourceModelPackage init() {
		if (isInited) return (TestSourceModelPackage)EPackage.Registry.INSTANCE.getEPackage(TestSourceModelPackage.eNS_URI);

		// Obtain or create and register package
		TestSourceModelPackageImpl theTestSourceModelPackage = (TestSourceModelPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof TestSourceModelPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new TestSourceModelPackageImpl());

		isInited = true;

		// Create package meta-data objects
		theTestSourceModelPackage.createPackageContents();

		// Initialize created meta-data
		theTestSourceModelPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theTestSourceModelPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(TestSourceModelPackage.eNS_URI, theTestSourceModelPackage);
		return theTestSourceModelPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getRootClass() {
		return rootClassEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getRootClass_AnAttribute1() {
		return (EAttribute)rootClassEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getRootClass_NormalReference() {
		return (EReference)rootClassEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getRootClass_Any() {
		return (EAttribute)rootClassEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getRootClass_NonManyReference() {
		return (EReference)rootClassEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getClassWithListFeatures() {
		return classWithListFeaturesEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getClassWithListFeatures_ListFeature1() {
		return (EReference)classWithListFeaturesEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getClassWithListFeatures_ListFeature2() {
		return (EReference)classWithListFeaturesEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getClassWithListFeatures_AnAttribute1() {
		return (EAttribute)classWithListFeaturesEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getListFeatureElementClass1() {
		return listFeatureElementClass1EClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getListFeatureElementClass1_Name() {
		return (EAttribute)listFeatureElementClass1EClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getListFeatureElementClass1_ListFeature3() {
		return (EReference)listFeatureElementClass1EClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getListFeatureElementClass1_AnAttributeOfFeatureClass1() {
		return (EAttribute)listFeatureElementClass1EClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getListFeatureElementClass1_Any() {
		return (EAttribute)listFeatureElementClass1EClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getListFeatureElementClass2() {
		return listFeatureElementClass2EClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getListFeatureElementClass2_Name() {
		return (EAttribute)listFeatureElementClass2EClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getListFeatureElementClass2_AnAttributeOfFeatureClass2() {
		return (EAttribute)listFeatureElementClass2EClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getListFeatureElementClass3() {
		return listFeatureElementClass3EClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getListFeatureElementClass3_Name() {
		return (EAttribute)listFeatureElementClass3EClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getListFeatureElementClass3_AnAttributeOfFeatureClass3() {
		return (EAttribute)listFeatureElementClass3EClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TestSourceModelFactory getTestSourceModelFactory() {
		return (TestSourceModelFactory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		rootClassEClass = createEClass(ROOT_CLASS);
		createEAttribute(rootClassEClass, ROOT_CLASS__AN_ATTRIBUTE1);
		createEReference(rootClassEClass, ROOT_CLASS__NORMAL_REFERENCE);
		createEAttribute(rootClassEClass, ROOT_CLASS__ANY);
		createEReference(rootClassEClass, ROOT_CLASS__NON_MANY_REFERENCE);

		classWithListFeaturesEClass = createEClass(CLASS_WITH_LIST_FEATURES);
		createEReference(classWithListFeaturesEClass, CLASS_WITH_LIST_FEATURES__LIST_FEATURE1);
		createEReference(classWithListFeaturesEClass, CLASS_WITH_LIST_FEATURES__LIST_FEATURE2);
		createEAttribute(classWithListFeaturesEClass, CLASS_WITH_LIST_FEATURES__AN_ATTRIBUTE1);

		listFeatureElementClass1EClass = createEClass(LIST_FEATURE_ELEMENT_CLASS1);
		createEAttribute(listFeatureElementClass1EClass, LIST_FEATURE_ELEMENT_CLASS1__NAME);
		createEReference(listFeatureElementClass1EClass, LIST_FEATURE_ELEMENT_CLASS1__LIST_FEATURE3);
		createEAttribute(listFeatureElementClass1EClass, LIST_FEATURE_ELEMENT_CLASS1__AN_ATTRIBUTE_OF_FEATURE_CLASS1);
		createEAttribute(listFeatureElementClass1EClass, LIST_FEATURE_ELEMENT_CLASS1__ANY);

		listFeatureElementClass2EClass = createEClass(LIST_FEATURE_ELEMENT_CLASS2);
		createEAttribute(listFeatureElementClass2EClass, LIST_FEATURE_ELEMENT_CLASS2__NAME);
		createEAttribute(listFeatureElementClass2EClass, LIST_FEATURE_ELEMENT_CLASS2__AN_ATTRIBUTE_OF_FEATURE_CLASS2);

		listFeatureElementClass3EClass = createEClass(LIST_FEATURE_ELEMENT_CLASS3);
		createEAttribute(listFeatureElementClass3EClass, LIST_FEATURE_ELEMENT_CLASS3__NAME);
		createEAttribute(listFeatureElementClass3EClass, LIST_FEATURE_ELEMENT_CLASS3__AN_ATTRIBUTE_OF_FEATURE_CLASS3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes

		// Initialize classes and features; add operations and parameters
		initEClass(rootClassEClass, RootClass.class, "RootClass", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getRootClass_AnAttribute1(), ecorePackage.getEString(), "anAttribute1", null, 0, 1, RootClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getRootClass_NormalReference(), this.getClassWithListFeatures(), null, "normalReference", null, 0, -1, RootClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getRootClass_Any(), ecorePackage.getEFeatureMapEntry(), "any", null, 0, -1, RootClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getRootClass_NonManyReference(), this.getClassWithListFeatures(), null, "nonManyReference", null, 0, 1, RootClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(classWithListFeaturesEClass, ClassWithListFeatures.class, "ClassWithListFeatures", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getClassWithListFeatures_ListFeature1(), this.getListFeatureElementClass1(), null, "listFeature1", null, 0, -1, ClassWithListFeatures.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getClassWithListFeatures_ListFeature2(), this.getListFeatureElementClass2(), null, "listFeature2", null, 0, -1, ClassWithListFeatures.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getClassWithListFeatures_AnAttribute1(), ecorePackage.getEInt(), "anAttribute1", null, 0, 1, ClassWithListFeatures.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(listFeatureElementClass1EClass, ListFeatureElementClass1.class, "ListFeatureElementClass1", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getListFeatureElementClass1_Name(), ecorePackage.getEString(), "name", null, 0, 1, ListFeatureElementClass1.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getListFeatureElementClass1_ListFeature3(), this.getListFeatureElementClass3(), null, "listFeature3", null, 0, -1, ListFeatureElementClass1.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getListFeatureElementClass1_AnAttributeOfFeatureClass1(), ecorePackage.getEString(), "anAttributeOfFeatureClass1", null, 0, 1, ListFeatureElementClass1.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getListFeatureElementClass1_Any(), ecorePackage.getEFeatureMapEntry(), "any", null, 0, -1, ListFeatureElementClass1.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(listFeatureElementClass2EClass, ListFeatureElementClass2.class, "ListFeatureElementClass2", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getListFeatureElementClass2_Name(), ecorePackage.getEString(), "name", null, 0, 1, ListFeatureElementClass2.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getListFeatureElementClass2_AnAttributeOfFeatureClass2(), ecorePackage.getEString(), "anAttributeOfFeatureClass2", null, 0, 1, ListFeatureElementClass2.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(listFeatureElementClass3EClass, ListFeatureElementClass3.class, "ListFeatureElementClass3", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getListFeatureElementClass3_Name(), ecorePackage.getEString(), "name", null, 0, 1, ListFeatureElementClass3.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getListFeatureElementClass3_AnAttributeOfFeatureClass3(), ecorePackage.getEString(), "anAttributeOfFeatureClass3", null, 0, 1, ListFeatureElementClass3.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// http:///org/eclipse/emf/ecore/util/ExtendedMetaData
		createExtendedMetaDataAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http:///org/eclipse/emf/ecore/util/ExtendedMetaData</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createExtendedMetaDataAnnotations() {
		String source = "http:///org/eclipse/emf/ecore/util/ExtendedMetaData";		
		addAnnotation
		  (getRootClass_Any(), 
		   source, 
		   new String[] {
			 "kind", "elementWildcard",
			 "name", ":1",
			 "processing", "lax",
			 "wildcards", "##any"
		   });		
		addAnnotation
		  (getListFeatureElementClass1_Any(), 
		   source, 
		   new String[] {
			 "kind", "elementWildcard",
			 "name", ":1",
			 "processing", "lax",
			 "wildcards", "##any"
		   });
	}

} //TestSourceModelPackageImpl
