package ec2

import "github.com/timob/javabind"

type ServicesEc2ModelStorageInterface interface {
	JavaLangObjectInterface

	// public void setS3(com.amazonaws.services.ec2.model.S3Storage)
	SetS3(a ServicesEc2ModelS3StorageInterface) 

	// public com.amazonaws.services.ec2.model.S3Storage getS3()
	GetS3() *ServicesEc2ModelS3Storage

	// public com.amazonaws.services.ec2.model.Storage withS3(com.amazonaws.services.ec2.model.S3Storage)
	WithS3(a ServicesEc2ModelS3StorageInterface) *ServicesEc2ModelStorage

	// public com.amazonaws.services.ec2.model.Storage clone()
	Clone() *ServicesEc2ModelStorage
}

type ServicesEc2ModelStorage struct {
	JavaLangObject
}

// public com.amazonaws.services.ec2.model.Storage()
func NewServicesEc2ModelStorage() (*ServicesEc2ModelStorage) {

	obj, err := javabind.GetEnv().NewObject("com/amazonaws/services/ec2/model/Storage")
	if err != nil {
		panic(err)
	}
	x := &ServicesEc2ModelStorage{}
	x.Callable = &javabind.Callable{obj}
	return x
}

// public void setS3(com.amazonaws.services.ec2.model.S3Storage)
func (jbobject *ServicesEc2ModelStorage) SetS3(a ServicesEc2ModelS3StorageInterface)  {
	conv_a := javabind.NewGoToJavaCallable()
	if err := conv_a.Convert(a); err != nil {
		panic(err)
	}
	_, err := jbobject.CallMethod(javabind.GetEnv(), "setS3", javabind.Void, conv_a.Value().Cast("com/amazonaws/services/ec2/model/S3Storage"))
	if err != nil {
		panic(err)
	}
	conv_a.CleanUp()

}

// public com.amazonaws.services.ec2.model.S3Storage getS3()
func (jbobject *ServicesEc2ModelStorage) GetS3() *ServicesEc2ModelS3Storage {
	jret, err := jbobject.CallMethod(javabind.GetEnv(), "getS3", "com/amazonaws/services/ec2/model/S3Storage")
	if err != nil {
		panic(err)
	}
	retconv := javabind.NewJavaToGoCallable()
	dst := &javabind.Callable{}
	retconv.Dest(dst)
	if err := retconv.Convert(javabind.ObjectRef(jret)); err != nil {
		panic(err)
	}
	retconv.CleanUp()
	unique_x := &ServicesEc2ModelS3Storage{}
	unique_x.Callable = dst
	return unique_x
}

// public com.amazonaws.services.ec2.model.Storage withS3(com.amazonaws.services.ec2.model.S3Storage)
func (jbobject *ServicesEc2ModelStorage) WithS3(a ServicesEc2ModelS3StorageInterface) *ServicesEc2ModelStorage {
	conv_a := javabind.NewGoToJavaCallable()
	if err := conv_a.Convert(a); err != nil {
		panic(err)
	}
	jret, err := jbobject.CallMethod(javabind.GetEnv(), "withS3", "com/amazonaws/services/ec2/model/Storage", conv_a.Value().Cast("com/amazonaws/services/ec2/model/S3Storage"))
	if err != nil {
		panic(err)
	}
	conv_a.CleanUp()
	retconv := javabind.NewJavaToGoCallable()
	dst := &javabind.Callable{}
	retconv.Dest(dst)
	if err := retconv.Convert(javabind.ObjectRef(jret)); err != nil {
		panic(err)
	}
	retconv.CleanUp()
	unique_x := &ServicesEc2ModelStorage{}
	unique_x.Callable = dst
	return unique_x
}

// public java.lang.String toString()
func (jbobject *ServicesEc2ModelStorage) ToString() string {
	jret, err := jbobject.CallMethod(javabind.GetEnv(), "toString", "java/lang/String")
	if err != nil {
		panic(err)
	}
	retconv := javabind.NewJavaToGoString()
	dst := new(string)
	retconv.Dest(dst)
	if err := retconv.Convert(javabind.ObjectRef(jret)); err != nil {
		panic(err)
	}
	retconv.CleanUp()
	return *dst
}

// public boolean equals(java.lang.Object)
func (jbobject *ServicesEc2ModelStorage) Equals(a interface{}) bool {
	conv_a := javabind.NewGoToJavaCallable()
	if err := conv_a.Convert(a); err != nil {
		panic(err)
	}
	jret, err := jbobject.CallMethod(javabind.GetEnv(), "equals", javabind.Boolean, conv_a.Value().Cast("java/lang/Object"))
	if err != nil {
		panic(err)
	}
	conv_a.CleanUp()
	return jret.(bool)
}

// public int hashCode()
func (jbobject *ServicesEc2ModelStorage) HashCode() int {
	jret, err := jbobject.CallMethod(javabind.GetEnv(), "hashCode", javabind.Int)
	if err != nil {
		panic(err)
	}
	return jret.(int)
}

// public com.amazonaws.services.ec2.model.Storage clone()
func (jbobject *ServicesEc2ModelStorage) Clone() *ServicesEc2ModelStorage {
	jret, err := jbobject.CallMethod(javabind.GetEnv(), "clone", "com/amazonaws/services/ec2/model/Storage")
	if err != nil {
		panic(err)
	}
	retconv := javabind.NewJavaToGoCallable()
	dst := &javabind.Callable{}
	retconv.Dest(dst)
	if err := retconv.Convert(javabind.ObjectRef(jret)); err != nil {
		panic(err)
	}
	retconv.CleanUp()
	unique_x := &ServicesEc2ModelStorage{}
	unique_x.Callable = dst
	return unique_x
}

// public java.lang.Object clone() throws java.lang.CloneNotSupportedException
func (jbobject *ServicesEc2ModelStorage) Clone2() (*JavaLangObject, error) {
	jret, err := jbobject.CallMethod(javabind.GetEnv(), "clone", "java/lang/Object")
	if err != nil {
		var zero *JavaLangObject
		return zero, err
	}
	retconv := javabind.NewJavaToGoCallable()
	dst := &javabind.Callable{}
	retconv.Dest(dst)
	if err := retconv.Convert(javabind.ObjectRef(jret)); err != nil {
		panic(err)
	}
	retconv.CleanUp()
	unique_x := &JavaLangObject{}
	unique_x.Callable = dst
	return unique_x, nil
}


