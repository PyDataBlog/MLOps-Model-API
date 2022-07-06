package scmirror

import org.specs2.mutable._

import scutil.Implicits._

// TODO merge with InnerTest
class OuterTest extends Specification {
	val clazz	= Class forName "scmirror.test.Outers$aborigines$"
	"Outers" should {
		"not have constructor parameters" in { 
			val constructor	= Reflector constructor clazz
			constructor mustEqual Some(List())
		}
		"inherit accessors" in { 
			val accessors	= Reflector accessors clazz
			accessors mustEqual List("id")
		}
	}
}
