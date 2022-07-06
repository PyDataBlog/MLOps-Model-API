A CMSViewDescription is a data structure to describe what information any CMSEmbeddedComponent shall contain and how it is supposed to be built.

Instance Variables
	buttons:		<OrderedCollection>
	generalStructure:		<OrderedCollection>
	header:		<String>
	tables:		<OrderedCollection >
	traceDescription:		<OrderedCollection>

buttons
	- xxxxx

generalStructure
	- the topics of the main box displayed at the top of each CMSEmbeddedComponent and 		its contents

header
	- the header of the CMSEmbeddedComponent being displayed above all its content

tables
	- all tables (each in an own box) being displayed at the CMSEmbeddedComponent.

traceDescription
	- a String and a symbol describing what to show in the tracer and where to go when this part of the tracer is clicked.
