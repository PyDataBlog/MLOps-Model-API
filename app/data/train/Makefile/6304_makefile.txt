# src/Makefile
include ../config/make.conf

all: CppExample  Tests ExecuteTests # ../obj/libUseEnumeration.a ../obj/libUseInheritance.a ../obj/libUseFactory.a ../obj/libUseVector.a ../obj/libUseDeque.a ../obj/libUseReference.a ../obj/libUseOverload.a ../obj/libUseBuilder.a ../obj/libUseSingleton.a

clean: cleanEnumeration cleanInheritance cleanFactory cleanVector cleanDeque cleanReference cleanOverload cleanBuilder cleanSingleton
	$(RMFORCE) CppExample
	$(RMFORCE) ../bin/CppExample

CppExample: CppExample.cpp ../obj/libUseEnumeration.a ../obj/libUseInheritance.a ../obj/libUseFactory.a ../obj/libUseVector.a ../obj/libUseDeque.a ../obj/libUseReference.a ../obj/libUseOverload.a ../obj/libUseBuilder.a ../obj/libUseSingleton.a
	$(CC) $(INC) CppExample.cpp $(LIBS) -o CppExample $(EXEFLAGS) #$(WFLAGS)
	cp CppExample ../bin/

Tests: ../bin/TriangleTest ../bin/CircleTest ../bin/RectangleTest

ExecuteTests: runShapeTests
	
#########################################
#####      The Type Examples        #####
#########################################

#########################################
##### Build the Enumeration Example #####
cleanEnumeration:
	$(RMFORCE) DataType/Enumeration/UseEnumeration.o
	$(RMFORCE) DataType/Enumeration/libUseEnumeration.a
	$(RMFORCE) ../obj/libUseEnumeration.a
	
../obj/libUseEnumeration.a: DataType/Enumeration/UseEnumeration.o
	ar rvs DataType/Enumeration/libUseEnumeration.a DataType/Enumeration/UseEnumeration.o
	cp DataType/Enumeration/libUseEnumeration.a ../obj
	
DataType/Enumeration/UseEnumeration.o: DataType/Enumeration/UseEnumeration.cpp
	$(CC) $(CFLAGS) $(INC) -o DataType/Enumeration/UseEnumeration.o DataType/Enumeration/UseEnumeration.cpp
	
####################################
##### Build the Vector Example #####
cleanVector:
	$(RMFORCE) DataType/Vector/UseVector.o
	$(RMFORCE) DataType/Vector/libUseVector.a
	$(RMFORCE) ../obj/libUseVector.a
	
../obj/libUseVector.a: DataType/Vector/UseVector.o ../obj/libUseInheritance.a ../obj/libUseEnumeration.a
	ar rvs DataType/Vector/libUseVector.a DataType/Vector/UseVector.o
	cp DataType/Vector/libUseVector.a ../obj
	
DataType/Vector/UseVector.o: DataType/Vector/UseVector.cpp
	$(CC) $(CFLAGS) $(INC) -o DataType/Vector/UseVector.o DataType/Vector/UseVector.cpp

####################################
##### Build the Deque Example  #####
cleanDeque:
	$(RMFORCE) DataType/Deque/UseDeque.o
	$(RMFORCE) DataType/Deque/libUseDeque.a
	$(RMFORCE) ../obj/libUseDeque.a
	
../obj/libUseDeque.a: DataType/Deque/UseDeque.o ../obj/libUseInheritance.a ../obj/libUseEnumeration.a
	ar rvs DataType/Deque/libUseDeque.a DataType/Deque/UseDeque.o
	cp DataType/Deque/libUseDeque.a ../obj
	
DataType/Deque/UseDeque.o: DataType/Deque/UseDeque.cpp
	$(CC) $(CFLAGS) $(INC) -o DataType/Deque/UseDeque.o DataType/Deque/UseDeque.cpp

#########################################
#####     The Concept Examples      #####
#########################################

#########################################
##### Build the Inheritance Example #####
cleanInheritance:
	$(RMFORCE) Concept/Inheritance/Shape.o
	$(RMFORCE) Concept/Inheritance/Circle.o
	$(RMFORCE) Concept/Inheritance/Rectangle.o
	$(RMFORCE) Concept/Inheritance/Triangle.o
	$(RMFORCE) Concept/Inheritance/libUseInheritance.a
	$(RMFORCE) ../obj/libUseInheritance.a
	$(RMFORCE) ../bin/TriangleTest
	$(RMFORCE) ../bin/CircleTest
	$(RMFORCE) ../bin/RectangleTest

../obj/libUseInheritance.a: $(SHAPEOBJS)
	ar rvs Concept/Inheritance/libUseInheritance.a $(SHAPEOBJS)
	cp Concept/Inheritance/libUseInheritance.a ../obj
	
Concept/Inheritance/Shape.o: Concept/Inheritance/Shape.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Inheritance/Shape.o Concept/Inheritance/Shape.cpp

Concept/Inheritance/Circle.o: Concept/Inheritance/Circle.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Inheritance/Circle.o Concept/Inheritance/Circle.cpp

Concept/Inheritance/Rectangle.o: Concept/Inheritance/Rectangle.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Inheritance/Rectangle.o Concept/Inheritance/Rectangle.cpp

Concept/Inheritance/Triangle.o: Concept/Inheritance/Triangle.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Inheritance/Triangle.o Concept/Inheritance/Triangle.cpp
	
../bin/TriangleTest: Concept/Inheritance/TriangleTest.cpp
	$(CC) $(INC) Concept/Inheritance/TriangleTest.cpp -L../obj -lgtest -lpthread -lUseInheritance -lUseEnumeration -o ../bin/TriangleTest $(EXEFLAGS) #$(WFLAGS)
	
../bin/CircleTest: Concept/Inheritance/CircleTest.cpp
	$(CC) $(INC) Concept/Inheritance/CircleTest.cpp -L../obj -lgtest -lpthread -lUseInheritance -lUseEnumeration -o ../bin/CircleTest $(EXEFLAGS) #$(WFLAGS)
	
../bin/RectangleTest: Concept/Inheritance/RectangleTest.cpp
	$(CC) $(INC) Concept/Inheritance/RectangleTest.cpp -L../obj -lgtest -lpthread -lUseInheritance -lUseEnumeration -o ../bin/RectangleTest $(EXEFLAGS) #$(WFLAGS)
	
####################################
##### Build the Reference Example #####
cleanReference:
	$(RMFORCE) Concept/Reference/UseReference.o
	$(RMFORCE) Concept/Reference/ObjectReference1.o
	$(RMFORCE) Concept/Reference/libUseReference.a
	$(RMFORCE) ../obj/libUseReference.a
	
../obj/libUseReference.a: Concept/Reference/UseReference.o Concept/Reference/ObjectReference1.o Concept/Reference/ObjectReference2.o
	ar rvs Concept/Reference/libUseReference.a Concept/Reference/UseReference.o Concept/Reference/ObjectReference1.o Concept/Reference/ObjectReference2.o
	cp Concept/Reference/libUseReference.a ../obj
	
Concept/Reference/UseReference.o: Concept/Reference/UseReference.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Reference/UseReference.o Concept/Reference/UseReference.cpp
	
Concept/Reference/ObjectReference1.o: Concept/Reference/ObjectReference1.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Reference/ObjectReference1.o Concept/Reference/ObjectReference1.cpp
	
Concept/Reference/ObjectReference2.o: Concept/Reference/ObjectReference2.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Reference/ObjectReference2.o Concept/Reference/ObjectReference2.cpp

####################################
##### Build the Overload Example #####
cleanOverload:
	$(RMFORCE) Concept/Overload/UseOverload.o
	$(RMFORCE) Concept/Overload/libUseOverload.a
	$(RMFORCE) ../obj/libUseOverload.a
	
../obj/libUseOverload.a: Concept/Overload/UseOverload.o
	ar rvs Concept/Overload/libUseOverload.a Concept/Overload/UseOverload.o
	cp Concept/Overload/libUseOverload.a ../obj
	
Concept/Overload/UseOverload.o: Concept/Overload/UseOverload.cpp
	$(CC) $(CFLAGS) $(INC) -o Concept/Overload/UseOverload.o Concept/Overload/UseOverload.cpp

#########################################
#####     The Pattern Examples      #####
#########################################

#####################################
##### Build the Factory Example #####
cleanFactory:
	$(RMFORCE) Pattern/Factory/UseFactory.o
	$(RMFORCE) Pattern/Factory/libUseFactory.a
	$(RMFORCE) ../obj/libUseFactory.a

../obj/libUseFactory.a: Pattern/Factory/UseFactory.o ../obj/libUseInheritance.a
	ar rvs Pattern/Factory/libUseFactory.a Pattern/Factory/UseFactory.o
	cp Pattern/Factory/libUseFactory.a ../obj
	
Pattern/Factory/UseFactory.o: Pattern/Factory/UseFactory.cpp
	$(CC) $(CFLAGS) $(INC) -o Pattern/Factory/UseFactory.o Pattern/Factory/UseFactory.cpp

#####################################
##### Build the Builder Example #####
cleanBuilder:
	$(RMFORCE) Pattern/Builder/UseBuilder.o
	$(RMFORCE) Pattern/Builder/libUseBuilder.a
	$(RMFORCE) ../obj/libUseBuilder.a

../obj/libUseBuilder.a: Pattern/Builder/TriangleBuilder.o Pattern/Builder/RectangleBuilder.o Pattern/Builder/ShapeBuilder.o ../obj/libUseInheritance.a
	ar rvs Pattern/Builder/libUseBuilder.a Pattern/Builder/ShapeBuilder.o Pattern/Builder/RectangleBuilder.o Pattern/Builder/TriangleBuilder.o
	cp Pattern/Builder/libUseBuilder.a ../obj
	
Pattern/Builder/ShapeBuilder.o: Pattern/Builder/ShapeBuilder.cpp
	$(CC) $(CFLAGS) $(INC) -o Pattern/Builder/ShapeBuilder.o Pattern/Builder/ShapeBuilder.cpp
	
Pattern/Builder/RectangleBuilder.o: Pattern/Builder/RectangleBuilder.cpp
	$(CC) $(CFLAGS) $(INC) -o Pattern/Builder/RectangleBuilder.o Pattern/Builder/RectangleBuilder.cpp
	
Pattern/Builder/TriangleBuilder.o: Pattern/Builder/TriangleBuilder.cpp
	$(CC) $(CFLAGS) $(INC) -o Pattern/Builder/TriangleBuilder.o Pattern/Builder/TriangleBuilder.cpp

#######################################
##### Build the Singleton Example #####
cleanSingleton:
	$(RMFORCE) Pattern/Singleton/UseSingleton.o
	$(RMFORCE) Pattern/Singleton/libUseSingleton.a
	$(RMFORCE) ../obj/libUseSingleton.a

../obj/libUseSingleton.a: Pattern/Singleton/UseSingleton.o
	ar rvs Pattern/Singleton/libUseSingleton.a Pattern/Singleton/UseSingleton.o
	cp Pattern/Singleton/libUseSingleton.a ../obj
	
Pattern/Singleton/UseSingleton.o: Pattern/Singleton/UseSingleton.cpp
	$(CC) $(CFLAGS) $(INC) -o Pattern/Singleton/UseSingleton.o Pattern/Singleton/UseSingleton.cpp

#########################
##### Run the Tests #####
#########################
runShapeTests:
	../bin/TriangleTest
	../bin/CircleTest
	../bin/RectangleTest
