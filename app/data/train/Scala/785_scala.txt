//-----------------------------------------------------------------------
// FILE    : TypeRepresentation.scala
// SUBJECT : Holds information about a type.
// AUTHOR  : (C) Copyright 2011 by Peter C. Chapin <PChapin@vtc.vsc.edu>
//
//-----------------------------------------------------------------------
package edu.uvm.sprocket

/**
 * Instances of this class store Sprocket's internal representation of a type. This is needed
 * when processing function parameters (and eventually in other places as well). Right now only
 * simple types are supported. Representing more complex types would be nice at some point but
 * since only (fairly) simple types can be used as duty parameters it is not necessary to worry
 * about complicated types at this time.
 */
class TypeRepresentation(val name: String)
