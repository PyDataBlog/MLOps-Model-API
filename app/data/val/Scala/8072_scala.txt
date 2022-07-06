package codeforces.round_282_div2

import test.utils.{StdoutTester, UnitSpec}

class C_Treasure$Test extends UnitSpec {
  val testable = C_Treasure.parseSolveAndPrint _

  "test_1" should "print correct output on test" in {
    StdoutTester {
      "(((#)((#)"
    } {
      "1\n2"
    } {
      testable
    }
  }

  "test_2" should "print correct output on test" in {
    StdoutTester {
      "()((#((#(#()"
    } {
      "1\n1\n3"
    } {
      testable
    }
  }

  "test_3" should "print correct output on test" in {
    StdoutTester {
      "#"
    } {
      "-1"
    } {
      testable
    }
  }

  "test_4" should "print correct output on test" in {
    StdoutTester {
      "(#)"
    } {
      "-1"
    } {
      testable
    }
  }

  "test_5" should "print correct output on test" in {
    StdoutTester {
      "((#(()#(##"
    } {
      "1\n1\n1\n1"
    } {
      testable
    }
  }

  "test_6" should "print correct output on test" in {
    StdoutTester {
      "##((((((()"
    } {
      "-1"
    } {
      testable
    }
  }

  "test_7" should "print correct output on test" in {
    StdoutTester {
      "(((((((((((((((((((###################"
    } {
      "1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1"
    } {
      testable
    }
  }

  "test_8" should "print correct output on test" in {
    StdoutTester {
      "((#)("
    } {
      "-1"
    } {
      testable
    }
  }

  "test_9" should "print correct output on test" in {
    StdoutTester {
      "((#)(((()"
    } {
      "-1"
    } {
      testable
    }
  }

  "test_10" should "print correct output on test" in {
    StdoutTester {
      "()#(#())()()#)(#)()##)#((()#)((#)()#())((#((((((((#)()()(()()(((((#)#(#((((#((##()(##(((#(()(#((#))#"
    } {
      "-1"
    } {
      testable
    }
  }

  "test_11" should "print correct output on test" in {
    StdoutTester {
      "(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)#(((((#)"
    } {
      "1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n130"
    } {
      testable
    }
  }
}
