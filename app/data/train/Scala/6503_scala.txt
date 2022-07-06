package net.thecoda.capnshinypants

class FullMonty[U <: reflect.api.Universe](val universe: U) extends CodeBuilderMixin {
  type Universe = U
}

object FullMonty {
  import reflect.runtime.{universe => ru}
  import scala.reflect.macros.blackbox.Context
  //note: whitebox subclasses blackbox


  def forRuntime = new FullMonty[ru.type](ru)
  def forMacro[C <: Context](ctx: C) = new FullMonty[ctx.universe.type](ctx.universe)
}