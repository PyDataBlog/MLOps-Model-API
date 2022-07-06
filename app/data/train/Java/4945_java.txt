/**
 */
package sc.ndt.editor.fast.fastfst;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>nTEC Sres</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link sc.ndt.editor.fast.fastfst.nTEC_Sres#getValue <em>Value</em>}</li>
 *   <li>{@link sc.ndt.editor.fast.fastfst.nTEC_Sres#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see sc.ndt.editor.fast.fastfst.FastfstPackage#getnTEC_Sres()
 * @model
 * @generated
 */
public interface nTEC_Sres extends EObject
{
  /**
   * Returns the value of the '<em><b>Value</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Value</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Value</em>' attribute.
   * @see #setValue(float)
   * @see sc.ndt.editor.fast.fastfst.FastfstPackage#getnTEC_Sres_Value()
   * @model
   * @generated
   */
  float getValue();

  /**
   * Sets the value of the '{@link sc.ndt.editor.fast.fastfst.nTEC_Sres#getValue <em>Value</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Value</em>' attribute.
   * @see #getValue()
   * @generated
   */
  void setValue(float value);

  /**
   * Returns the value of the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Name</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Name</em>' attribute.
   * @see #setName(String)
   * @see sc.ndt.editor.fast.fastfst.FastfstPackage#getnTEC_Sres_Name()
   * @model
   * @generated
   */
  String getName();

  /**
   * Sets the value of the '{@link sc.ndt.editor.fast.fastfst.nTEC_Sres#getName <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' attribute.
   * @see #getName()
   * @generated
   */
  void setName(String value);

} // nTEC_Sres
