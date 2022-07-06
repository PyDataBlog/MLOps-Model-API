package com.negrisoli.algorithms.implementation

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import BiggerIsGreater._

import BiggerIsGreater._

@RunWith(classOf[JUnitRunner])
class BiggerIsGreaterTest extends FunSuite {
  
  test("test 'ab'") {
    assert(solve("ab") === "ba")
  }
  
  test("test 'bb'") {
    assert(solve("bb") === "no answer")
  }
  
  test("test 'hefg'") {
    assert(solve("hefg") === "hegf")
  }
  
  test("test 'dkhc'") {
    assert(solve("dkhc") === "hcdk")
  }
  
  test("test 'dldmsym'") {
    assert(solve("dldmsym") === "dldmyms")
  }
}
