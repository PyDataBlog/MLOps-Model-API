package org.precompiler.scala101

import org.precompiler.scala101.ch04.Currency.Currency

/**
  *
  * @author Richard Li
  */
package object ch04 {
  def convert(money: Money, to: Currency) = {
    println(s"converting ${money} to ${to}")
  }
}
