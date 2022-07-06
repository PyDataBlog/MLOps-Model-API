------------------------------------------------------------------------

<span id="navbar_top"></span> [](#skip-navbar_top "Skip navigation links")

<table>
<colgroup>
<col width="50%" />
<col width="50%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="left"><span id="navbar_top_firstrow"></span>
<table>
<tbody>
<tr class="odd">
<td align="left"><a href="../../../../overview-summary.html.md"><strong>Overview</strong></a> </td>
<td align="left"><a href="package-summary.html.md"><strong>Package</strong></a> </td>
<td align="left"> <strong>Class</strong> </td>
<td align="left"><a href="class-use/ActionMessages.html.md"><strong>Use</strong></a> </td>
<td align="left"><a href="package-tree.html.md"><strong>Tree</strong></a> </td>
<td align="left"><a href="../../../../deprecated-list.html.md"><strong>Deprecated</strong></a> </td>
<td align="left"><a href="../../../../index-all.html.md"><strong>Index</strong></a> </td>
<td align="left"><a href="../../../../help-doc.html.md"><strong>Help</strong></a> </td>
</tr>
</tbody>
</table></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"> <a href="../../../../org/apache/struts/action/ActionMessage.html.md" title="class in org.apache.struts.action"><strong>PREV CLASS</strong></a>   <a href="../../../../org/apache/struts/action/ActionMessages.ActionMessageItem.html" title="class in org.apache.struts.action"><strong>NEXT CLASS</strong></a></td>
<td align="left"><a href="../../../../index.html.md?org/apache/struts/action/ActionMessages.html"><strong>FRAMES</strong></a>    <a href="ActionMessages.html"><strong>NO FRAMES</strong></a>    
<a href="../../../../allclasses-noframe.html.md"><strong>All Classes</strong></a></td>
</tr>
<tr class="odd">
<td align="left">SUMMARY: <a href="#nested_class_summary">NESTED</a> | <a href="#field_summary">FIELD</a> | <a href="#constructor_summary">CONSTR</a> | <a href="#method_summary">METHOD</a></td>
<td align="left">DETAIL: <a href="#field_detail">FIELD</a> | <a href="#constructor_detail">CONSTR</a> | <a href="#method_detail">METHOD</a></td>
</tr>
</tbody>
</table>

<span id="skip-navbar_top"></span>

------------------------------------------------------------------------

org.apache.struts.action
 Class ActionMessages
------------------------

    java.lang.Object
      org.apache.struts.action.ActionMessages

**All Implemented Interfaces:**  
[Serializable](http://java.sun.com/j2se/1.4.2/docs/api/java/io/Serializable.html.md?is-external=true "class or interface in java.io")

<!-- -->

**Direct Known Subclasses:**  
[ActionErrors](../../../../org/apache/struts/action/ActionErrors.html.md "class in org.apache.struts.action")

------------------------------------------------------------------------

    public class ActionMessages

extends [Object](http://java.sun.com/j2se/1.4.2/docs/api/java/lang/Object.html.md?is-external=true "class or interface in java.lang")

implements [Serializable](http://java.sun.com/j2se/1.4.2/docs/api/java/io/Serializable.html.md?is-external=true "class or interface in java.io")

A class that encapsulates messages. Messages can be either global or they are specific to a particular bean property.

Each individual message is described by an `ActionMessage` object, which contains a message key (to be looked up in an appropriate message resources database), and up to four placeholder arguments used for parametric substitution in the resulting message.

**IMPLEMENTATION NOTE** - It is assumed that these objects are created and manipulated only within the context of a single thread. Therefore, no synchronization is required for access to internal collections.

**Since:**  
Struts 1.1

**Version:**  
$Rev: 471754 $ $Date: 2005-08-26 21:58:39 -0400 (Fri, 26 Aug 2005) $

**See Also:**  
[Serialized Form](../../../../serialized-form.html.md#org.apache.struts.action.ActionMessages)

------------------------------------------------------------------------

<span id="nested_class_summary"></span>

**Nested Class Summary**

`protected  class`

`ActionMessages.ActionMessageItem`
           This class is used to store a set of messages associated with a property/key and the position it was initially added to list.

  <span id="field_summary"></span>

**Field Summary**

`protected  boolean`

` accessed`
           Have the messages been retrieved from this object?

`static String`

` GLOBAL_MESSAGE`
           The "property name" marker to use for global messages, as opposed to those related to a specific property.

`protected  int`

`iCount`
           The current number of the property/key being added.

`protected  HashMap`

` messages`
           The accumulated set of `ActionMessage` objects (represented as an ArrayList) for each property, keyed by property name.

  <span id="constructor_summary"></span>

| **Constructor Summary**                                                           |
|-----------------------------------------------------------------------------------|
| ` ActionMessages()`                                                               
            Create an empty `ActionMessages` object.                                |
| ` ActionMessages(ActionMessages messages)`                                        
            Create an `ActionMessages` object initialized with the given messages.  |

  <span id="method_summary"></span>

**Method Summary**

` void`

` add(ActionMessages actionMessages)`
           Adds the messages from the given `ActionMessages` object to this set of messages.

` void`

` add(String property, ActionMessage message)`
           Add a message to the set of messages for the specified property.

` void`

`clear()`
           Clear all messages recorded by this object.

` Iterator`

`get()`
           Return the set of all recorded messages, without distinction by which property the messages are associated with.

` Iterator`

` get(String property)`
           Return the set of messages related to a specific property.

` boolean`

` isAccessed()`
           Returns `true` if the `get()` or `get(String)` methods are called.

` boolean`

` isEmpty()`
           Return `true` if there are no messages recorded in this collection, or `false` otherwise.

` Iterator`

` properties()`
           Return the set of property names for which at least one message has been recorded.

` int`

`size()`
           Return the number of messages recorded for all properties (including global messages).

` int`

` size(String property)`
           Return the number of messages associated with the specified property.

` String`

` toString()`
           Returns a String representation of this ActionMessages' property name=message list mapping.

 <span id="methods_inherited_from_class_java.lang.Object"></span>

| **Methods inherited from class java.lang.[Object](http://java.sun.com/j2se/1.4.2/docs/api/java/lang/Object.html.md?is-external=true "class or interface in java.lang")** |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `clone, equals, finalize, getClass, hashCode, notify, notifyAll, wait, wait, wait`                                                                                    |

 

<span id="field_detail"></span>

**Field Detail**

<span id="GLOBAL_MESSAGE"></span>

### GLOBAL\_MESSAGE

    public static final String GLOBAL_MESSAGE

The "property name" marker to use for global messages, as opposed to those related to a specific property.

**See Also:**  
[Constant Field Values](../../../../constant-values.html.md#org.apache.struts.action.ActionMessages.GLOBAL_MESSAGE)

------------------------------------------------------------------------

<span id="accessed"></span>

### accessed

    protected boolean accessed

Have the messages been retrieved from this object?

The controller uses this property to determine if session-scoped messages can be removed.

**Since:**  
Struts 1.2

------------------------------------------------------------------------

<span id="messages"></span>

### messages

    protected HashMap messages

The accumulated set of `ActionMessage` objects (represented as an ArrayList) for each property, keyed by property name.

------------------------------------------------------------------------

<span id="iCount"></span>

### iCount

    protected int iCount

The current number of the property/key being added. This is used to maintain the order messages are added.

<span id="constructor_detail"></span>

**Constructor Detail**

### ActionMessages

    public ActionMessages()

Create an empty `ActionMessages` object.

------------------------------------------------------------------------

### ActionMessages

    public ActionMessages(ActionMessages messages)

Create an `ActionMessages` object initialized with the given messages.

**Parameters:**  
`messages` - The messages to be initially added to this object. This parameter can be `null`.

**Since:**  
Struts 1.1

<span id="method_detail"></span>

**Method Detail**

### add

    public void add(String property,
                    ActionMessage message)

Add a message to the set of messages for the specified property. An order of the property/key is maintained based on the initial addition of the property/key.

**Parameters:**  
`property` - Property name (or ActionMessages.GLOBAL\_MESSAGE)

`message` - The message to be added

------------------------------------------------------------------------

### add

    public void add(ActionMessages actionMessages)

Adds the messages from the given `ActionMessages` object to this set of messages. The messages are added in the order they are returned from the `properties` method. If a message's property is already in the current `ActionMessages` object, it is added to the end of the list for that property. If a message's property is not in the current list it is added to the end of the properties.

**Parameters:**  
`actionMessages` - The `ActionMessages` object to be added. This parameter can be `null`.

**Since:**  
Struts 1.1

------------------------------------------------------------------------

### clear

    public void clear()

Clear all messages recorded by this object.

------------------------------------------------------------------------

### isEmpty

    public boolean isEmpty()

Return `true` if there are no messages recorded in this collection, or `false` otherwise.

**Returns:**  
`true` if there are no messages recorded in this collection; `false` otherwise.

**Since:**  
Struts 1.1

------------------------------------------------------------------------

### get

    public Iterator get()

Return the set of all recorded messages, without distinction by which property the messages are associated with. If there are no messages recorded, an empty enumeration is returned.

**Returns:**  
An iterator over the messages for all properties.

------------------------------------------------------------------------

### get

    public Iterator get(String property)

Return the set of messages related to a specific property. If there are no such messages, an empty enumeration is returned.

**Parameters:**  
`property` - Property name (or ActionMessages.GLOBAL\_MESSAGE)

**Returns:**  
An iterator over the messages for the specified property.

------------------------------------------------------------------------

### isAccessed

    public boolean isAccessed()

Returns `true` if the `get()` or `get(String)` methods are called.

**Returns:**  
`true` if the messages have been accessed one or more times.

**Since:**  
Struts 1.2

------------------------------------------------------------------------

### properties

    public Iterator properties()

Return the set of property names for which at least one message has been recorded. If there are no messages, an empty `Iterator` is returned. If you have recorded global messages, the `String` value of `ActionMessages.GLOBAL_MESSAGE` will be one of the returned property names.

**Returns:**  
An iterator over the property names for which messages exist.

------------------------------------------------------------------------

### size

    public int size()

Return the number of messages recorded for all properties (including global messages). **NOTE** - it is more efficient to call `isEmpty` if all you care about is whether or not there are any messages at all.

**Returns:**  
The number of messages associated with all properties.

------------------------------------------------------------------------

### size

    public int size(String property)

Return the number of messages associated with the specified property.

**Parameters:**  
`property` - Property name (or ActionMessages.GLOBAL\_MESSAGE)

**Returns:**  
The number of messages associated with the property.

------------------------------------------------------------------------

### toString

    public String toString()

Returns a String representation of this ActionMessages' property name=message list mapping.

**Overrides:**  
`toString` in class `Object`

<!-- -->

**Returns:**  
String representation of the messages

**See Also:**  
[`Object.toString()`](http://java.sun.com/j2se/1.4.2/docs/api/java/lang/Object.html.md?is-external=true#toString() "class or interface in java.lang")

------------------------------------------------------------------------

<span id="navbar_bottom"></span> [](#skip-navbar_bottom "Skip navigation links")

<table>
<colgroup>
<col width="50%" />
<col width="50%" />
</colgroup>
<tbody>
<tr class="odd">
<td align="left"><span id="navbar_bottom_firstrow"></span>
<table>
<tbody>
<tr class="odd">
<td align="left"><a href="../../../../overview-summary.html.md"><strong>Overview</strong></a> </td>
<td align="left"><a href="package-summary.html.md"><strong>Package</strong></a> </td>
<td align="left"> <strong>Class</strong> </td>
<td align="left"><a href="class-use/ActionMessages.html.md"><strong>Use</strong></a> </td>
<td align="left"><a href="package-tree.html.md"><strong>Tree</strong></a> </td>
<td align="left"><a href="../../../../deprecated-list.html.md"><strong>Deprecated</strong></a> </td>
<td align="left"><a href="../../../../index-all.html.md"><strong>Index</strong></a> </td>
<td align="left"><a href="../../../../help-doc.html.md"><strong>Help</strong></a> </td>
</tr>
</tbody>
</table></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"> <a href="../../../../org/apache/struts/action/ActionMessage.html.md" title="class in org.apache.struts.action"><strong>PREV CLASS</strong></a>   <a href="../../../../org/apache/struts/action/ActionMessages.ActionMessageItem.html" title="class in org.apache.struts.action"><strong>NEXT CLASS</strong></a></td>
<td align="left"><a href="../../../../index.html.md?org/apache/struts/action/ActionMessages.html"><strong>FRAMES</strong></a>    <a href="ActionMessages.html"><strong>NO FRAMES</strong></a>    
<a href="../../../../allclasses-noframe.html.md"><strong>All Classes</strong></a></td>
</tr>
<tr class="odd">
<td align="left">SUMMARY: <a href="#nested_class_summary">NESTED</a> | <a href="#field_summary">FIELD</a> | <a href="#constructor_summary">CONSTR</a> | <a href="#method_summary">METHOD</a></td>
<td align="left">DETAIL: <a href="#field_detail">FIELD</a> | <a href="#constructor_detail">CONSTR</a> | <a href="#method_detail">METHOD</a></td>
</tr>
</tbody>
</table>

<span id="skip-navbar_bottom"></span>

------------------------------------------------------------------------

Copyright © 2000-2008 [Apache Software Foundation](http://www.apache.org/). All Rights Reserved.
