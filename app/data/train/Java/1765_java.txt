/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

import de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.Cpu_usage;
import de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.Sys_info_systeminfoPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Cpu usage</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.impl.Cpu_usageImpl#getEContainer_cpu_usage <em>EContainer cpu usage</em>}</li>
 *   <li>{@link de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.impl.Cpu_usageImpl#getReal <em>Real</em>}</li>
 *   <li>{@link de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.impl.Cpu_usageImpl#getUser <em>User</em>}</li>
 *   <li>{@link de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.impl.Cpu_usageImpl#getSys <em>Sys</em>}</li>
 *   <li>{@link de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.impl.Cpu_usageImpl#getUnit <em>Unit</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class Cpu_usageImpl extends EObjectImpl implements Cpu_usage {
    /**
     * The default value of the '{@link #getReal() <em>Real</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getReal()
     * @generated
     * @ordered
     */
    protected static final double REAL_EDEFAULT = 0.0;

    /**
     * The cached value of the '{@link #getReal() <em>Real</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getReal()
     * @generated
     * @ordered
     */
    protected double real = REAL_EDEFAULT;

    /**
     * The default value of the '{@link #getUser() <em>User</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getUser()
     * @generated
     * @ordered
     */
    protected static final double USER_EDEFAULT = 0.0;

    /**
     * The cached value of the '{@link #getUser() <em>User</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getUser()
     * @generated
     * @ordered
     */
    protected double user = USER_EDEFAULT;

    /**
     * The default value of the '{@link #getSys() <em>Sys</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getSys()
     * @generated
     * @ordered
     */
    protected static final double SYS_EDEFAULT = 0.0;

    /**
     * The cached value of the '{@link #getSys() <em>Sys</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getSys()
     * @generated
     * @ordered
     */
    protected double sys = SYS_EDEFAULT;

    /**
     * The default value of the '{@link #getUnit() <em>Unit</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getUnit()
     * @generated
     * @ordered
     */
    protected static final String UNIT_EDEFAULT = null;

    /**
     * The cached value of the '{@link #getUnit() <em>Unit</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getUnit()
     * @generated
     * @ordered
     */
    protected String unit = UNIT_EDEFAULT;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected Cpu_usageImpl() {
        super();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    protected EClass eStaticClass() {
        return Sys_info_systeminfoPackage.Literals.CPU_USAGE;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System getEContainer_cpu_usage() {
        if (eContainerFeatureID() != Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE) return null;
        return (de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System)eContainer();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public NotificationChain basicSetEContainer_cpu_usage(de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System newEContainer_cpu_usage, NotificationChain msgs) {
        msgs = eBasicSetContainer((InternalEObject)newEContainer_cpu_usage, Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE, msgs);
        return msgs;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setEContainer_cpu_usage(de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System newEContainer_cpu_usage) {
        if (newEContainer_cpu_usage != eInternalContainer() || (eContainerFeatureID() != Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE && newEContainer_cpu_usage != null)) {
            if (EcoreUtil.isAncestor(this, newEContainer_cpu_usage))
                throw new IllegalArgumentException("Recursive containment not allowed for " + toString());
            NotificationChain msgs = null;
            if (eInternalContainer() != null)
                msgs = eBasicRemoveFromContainer(msgs);
            if (newEContainer_cpu_usage != null)
                msgs = ((InternalEObject)newEContainer_cpu_usage).eInverseAdd(this, Sys_info_systeminfoPackage.SYSTEM__CPU_USAGE, de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System.class, msgs);
            msgs = basicSetEContainer_cpu_usage(newEContainer_cpu_usage, msgs);
            if (msgs != null) msgs.dispatch();
        }
        else if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE, newEContainer_cpu_usage, newEContainer_cpu_usage));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public double getReal() {
        return real;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setReal(double newReal) {
        double oldReal = real;
        real = newReal;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Sys_info_systeminfoPackage.CPU_USAGE__REAL, oldReal, real));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public double getUser() {
        return user;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setUser(double newUser) {
        double oldUser = user;
        user = newUser;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Sys_info_systeminfoPackage.CPU_USAGE__USER, oldUser, user));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public double getSys() {
        return sys;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setSys(double newSys) {
        double oldSys = sys;
        sys = newSys;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Sys_info_systeminfoPackage.CPU_USAGE__SYS, oldSys, sys));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public String getUnit() {
        return unit;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setUnit(String newUnit) {
        String oldUnit = unit;
        unit = newUnit;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, Sys_info_systeminfoPackage.CPU_USAGE__UNIT, oldUnit, unit));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE:
                if (eInternalContainer() != null)
                    msgs = eBasicRemoveFromContainer(msgs);
                return basicSetEContainer_cpu_usage((de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System)otherEnd, msgs);
        }
        return super.eInverseAdd(otherEnd, featureID, msgs);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE:
                return basicSetEContainer_cpu_usage(null, msgs);
        }
        return super.eInverseRemove(otherEnd, featureID, msgs);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
        switch (eContainerFeatureID()) {
            case Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE:
                return eInternalContainer().eInverseRemove(this, Sys_info_systeminfoPackage.SYSTEM__CPU_USAGE, de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System.class, msgs);
        }
        return super.eBasicRemoveFromContainerFeature(msgs);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE:
                return getEContainer_cpu_usage();
            case Sys_info_systeminfoPackage.CPU_USAGE__REAL:
                return getReal();
            case Sys_info_systeminfoPackage.CPU_USAGE__USER:
                return getUser();
            case Sys_info_systeminfoPackage.CPU_USAGE__SYS:
                return getSys();
            case Sys_info_systeminfoPackage.CPU_USAGE__UNIT:
                return getUnit();
        }
        return super.eGet(featureID, resolve, coreType);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public void eSet(int featureID, Object newValue) {
        switch (featureID) {
            case Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE:
                setEContainer_cpu_usage((de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System)newValue);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__REAL:
                setReal((Double)newValue);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__USER:
                setUser((Double)newValue);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__SYS:
                setSys((Double)newValue);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__UNIT:
                setUnit((String)newValue);
                return;
        }
        super.eSet(featureID, newValue);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public void eUnset(int featureID) {
        switch (featureID) {
            case Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE:
                setEContainer_cpu_usage((de.hub.clickwatch.specificmodels.brn.sys_info_systeminfo.System)null);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__REAL:
                setReal(REAL_EDEFAULT);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__USER:
                setUser(USER_EDEFAULT);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__SYS:
                setSys(SYS_EDEFAULT);
                return;
            case Sys_info_systeminfoPackage.CPU_USAGE__UNIT:
                setUnit(UNIT_EDEFAULT);
                return;
        }
        super.eUnset(featureID);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public boolean eIsSet(int featureID) {
        switch (featureID) {
            case Sys_info_systeminfoPackage.CPU_USAGE__ECONTAINER_CPU_USAGE:
                return getEContainer_cpu_usage() != null;
            case Sys_info_systeminfoPackage.CPU_USAGE__REAL:
                return real != REAL_EDEFAULT;
            case Sys_info_systeminfoPackage.CPU_USAGE__USER:
                return user != USER_EDEFAULT;
            case Sys_info_systeminfoPackage.CPU_USAGE__SYS:
                return sys != SYS_EDEFAULT;
            case Sys_info_systeminfoPackage.CPU_USAGE__UNIT:
                return UNIT_EDEFAULT == null ? unit != null : !UNIT_EDEFAULT.equals(unit);
        }
        return super.eIsSet(featureID);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public String toString() {
        if (eIsProxy()) return super.toString();

        StringBuffer result = new StringBuffer(super.toString());
        result.append(" (real: ");
        result.append(real);
        result.append(", user: ");
        result.append(user);
        result.append(", sys: ");
        result.append(sys);
        result.append(", unit: ");
        result.append(unit);
        result.append(')');
        return result.toString();
    }

} //Cpu_usageImpl
