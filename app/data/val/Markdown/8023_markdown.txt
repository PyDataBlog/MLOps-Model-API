
    1   /*
    2    * $Id: TestActionRedirect.java 514052 2007-03-03 02:00:37Z pbenedict $
    3    *
    4    * Licensed to the Apache Software Foundation (ASF) under one
    5    * or more contributor license agreements.  See the NOTICE file
    6    * distributed with this work for additional information
    7    * regarding copyright ownership.  The ASF licenses this file
    8    * to you under the Apache License, Version 2.0 (the
    9    * "License"); you may not use this file except in compliance
    10   * with the License.  You may obtain a copy of the License at
    11   *
    12   *  http://www.apache.org/licenses/LICENSE-2.0
    13   *
    14   * Unless required by applicable law or agreed to in writing,
    15   * software distributed under the License is distributed on an
    16   * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
    17   * KIND, either express or implied.  See the License for the
    18   * specific language governing permissions and limitations
    19   * under the License.
    20   */
    21  package org.apache.struts.action;
    22  
    23  import junit.framework.AssertionFailedError;
    24  import junit.framework.ComparisonFailure;
    25  import junit.framework.TestCase;
    26  import junit.framework.TestSuite;
    27  
    28  import java.util.Map;
    29  
    30  /**
    31   * <p>Unit tests for the {@link ActionRedirect} class.</p>
    32   *
    33   * @version $Rev: 514052 $ $Date: 2007-03-02 20:00:37 -0600 (Fri, 02 Mar 2007) $
    34   */
    35  public class TestActionRedirect extends TestCase {
    36      public TestActionRedirect(String s) {
    37          super(s);
    38      }
    39  
    40      public static TestSuite getSuite() {
    41          return new TestSuite(TestActionRedirect.class);
    42      }
    43  
    44      public static void main(String[] args) {
    45          junit.textui.TestRunner runner = new junit.textui.TestRunner();
    46  
    47          runner.doRun(TestActionRedirect.getSuite());
    48      }
    49  
    50      // ----------------------------------------------------- Test Methods
    51  
    52      /**
    53       * Check that the redirect flag is set.
    54       */
    55      public void testActionRedirectRedirectFlag() {
    56          ActionRedirect ar = new ActionRedirect("/path.do");
    57  
    58          assertTrue("Redirect flag should be set to true.", ar.getRedirect());
    59      }
    60  
    61      /**
    62       * Test all addParameter methods accepting different data types.
    63       */
    64      public void testActionRedirectAddParameter() {
    65          ActionRedirect ar = new ActionRedirect("/path.do");
    66  
    67          ar.addParameter("st", "test");
    68          ar.addParameter("obj", new StringBuffer("someString"));
    69  
    70          assertTrue("Incorrect path", ar.getPath().indexOf("/path.do") == 0);
    71          assertHasParameter(ar.parameterValues, "st", "test");
    72          assertHasParameter(ar.parameterValues, "obj", "someString");
    73      }
    74  
    75      /**
    76       * Test redirect with anchor.
    77       */
    78      public void testActionRedirectWithAnchor() {
    79          ActionRedirect ar = new ActionRedirect("/path.do");
    80  
    81          ar.addParameter("st", "test");
    82          ar.setAnchor("foo");
    83  
    84          assertTrue("Incorrect path", "/path.do?st=test#foo".equals(ar.getPath()));
    85      }
    86  
    87      /**
    88       * Test adding parameters with the same name.
    89       */
    90      public void testActionRedirectAddSameNameParameter() {
    91          ActionRedirect ar = new ActionRedirect("/path.do");
    92  
    93          ar.addParameter("param", "param1");
    94          ar.addParameter("param", "param2");
    95          ar.addParameter("param", new StringBuffer("someString"));
    96  
    97          assertTrue("Incorrect path", ar.getPath().indexOf("/path.do") == 0);
    98          assertHasParameter(ar.parameterValues, "param", "param1");
    99          assertHasParameter(ar.parameterValues, "param", "param2");
    100         assertHasParameter(ar.parameterValues, "param", "someString");
    101         assertEquals("Incorrect number of parameters", 3,
    102             countParameters(ar.parameterValues, "param"));
    103     }
    104 
    105     /**
    106      * Test creating an ActionRedirect which copies its configuration from an
    107      * existing ActionForward (except for the "redirect" property).
    108      */
    109     public void testActionRedirectFromExistingForward() {
    110         ActionForward forward = new ActionForward("/path.do?param=param1");
    111         forward.setRedirect(false);
    112         forward.setProperty("key","value");
    113 
    114         ActionRedirect ar = new ActionRedirect(forward);
    115 
    116         ar.addParameter("param", "param2");
    117         ar.addParameter("object1", new StringBuffer("someString"));
    118 
    119         assertTrue("Incorrect path", ar.getPath().indexOf("/path.do") == 0);
    120         assertHasParameter(ar.parameterValues, "param", "param2");
    121         assertHasParameter(ar.parameterValues, "object1", "someString");
    122         assertEquals("Incorrect original path.", forward.getPath(),
    123             ar.getOriginalPath());
    124         assertEquals("Incorrect or missing property", "value",
    125             ar.getProperty("key"));
    126         assertTrue("Original had redirect to false", ar.getRedirect());
    127     }
    128 
    129     /**
    130      * Assert that the given parameters contains an entry for
    131      * <code>paramValue</code> under the <code>paramName</code> key. <p/>
    132      *
    133      * @param parameters the map of parameters to check into
    134      * @param paramName  the key of the value to be checked
    135      * @param paramValue the value to check for
    136      */
    137     static void assertHasParameter(Map parameters, String paramName,
    138         String paramValue) {
    139         Object value = parameters.get(paramName);
    140 
    141         if (value == null) {
    142             throw new AssertionFailedError("Parameter [" + paramName
    143                 + "] not found");
    144         }
    145 
    146         if (value instanceof String) {
    147             if (!paramValue.equals(value)) {
    148                 throw new ComparisonFailure("Incorrect value found",
    149                     paramValue, (String) value);
    150             }
    151         } else if (value instanceof String[]) {
    152             // see if our value is among those in the array
    153             String[] values = (String[]) value;
    154 
    155             for (int i = 0; i < values.length; i++) {
    156                 if (paramValue.equals(values[i])) {
    157                     return;
    158                 }
    159             }
    160 
    161             throw new AssertionFailedError(
    162                 "Expected value not found for parameter [" + paramName + "]");
    163         } else {
    164             // can't recognize the value
    165             throw new AssertionFailedError(
    166                 "Unexpected type found as parameter value for [" + paramName
    167                 + "]");
    168         }
    169     }
    170 
    171     /**
    172      * Determine the number of values that are available for a specific
    173      * parameter. <p/>
    174      *
    175      * @param parameters the map of parameters to check into
    176      * @param paramName  the key of the value(s) to count
    177      * @return the number of values for the specified parameter
    178      */
    179     static int countParameters(Map parameters, String paramName) {
    180         Object value = parameters.get(paramName);
    181 
    182         if (value == null) {
    183             return 0;
    184         }
    185 
    186         if (value instanceof String) {
    187             return 1;
    188         } else if (value instanceof String[]) {
    189             String[] values = (String[]) value;
    190 
    191             return values.length;
    192         } else {
    193             // can't recognize the value
    194             throw new AssertionFailedError(
    195                 "Unexpected type found as parameter value for [" + paramName
    196                 + "]");
    197         }
    198     }
    199 }

------------------------------------------------------------------------

This page was automatically generated by [Maven](http://maven.apache.org/)
