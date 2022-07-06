
package views.html

import play.templates._
import play.templates.TemplateMagic._

import play.api.templates._
import play.api.templates.PlayMagic._
import models._
import controllers._
import java.lang._
import java.util._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import play.api.i18n._
import play.core.j.PlayMagicForJava._
import play.mvc._
import play.data._
import play.api.data.Field
import play.mvc.Http.Context.Implicit._
import views.html._
/**/
object Joyce extends BaseScalaTemplate[play.api.templates.HtmlFormat.Appendable,Format[play.api.templates.HtmlFormat.Appendable]](play.api.templates.HtmlFormat) with play.api.templates.Template0[play.api.templates.HtmlFormat.Appendable] {

    /**/
    def apply():play.api.templates.HtmlFormat.Appendable = {
        _display_ {

Seq[Any](_display_(Seq[Any](/*1.2*/Main("Joyce Hoffman")/*1.23*/ {_display_(Seq[Any](format.raw/*1.25*/("""
<div class="container">
  <div class="row" style="margin-top: 20px">
    <div class="col-md-4">
      <div class="well">
        <div class="joyce-img2"></div>
      </div>
    </div>
    <div class="col-md-8">
      <div class="well">
        <h3>Joyce Hoffman</h3>
        <p>Joyce Hoffman (born 1946/47) is an American surfer, considered a woman's pioneer in her sport. She is
          often regarded as the first female international surfing star and was one the first inductees of the
          International Surfing Hall of Fame.</p>

        <p>Born in Dana Point, California, Hoffman started competing at an early age. She went on to win numerous
          honors including the US Surfing Championship for Women from 1965-67 and the Makaha International Open in 1964
          and 1966. In 1968, she became the first female to surf the Banzai Pipeline in Hawaii. Hoffman was also the top
          vote getter for Surfing Magazine International Hall of Fame Awards in 1966 and 1967. In 1969, the Los Angeles
          Times named her Woman of the Year, the only surfer to ever hold this distinction.</p>
      </div>
    </div>
  </div>
</div>
""")))})),format.raw/*25.2*/("""
"""))}
    }
    
    def render(): play.api.templates.HtmlFormat.Appendable = apply()
    
    def f:(() => play.api.templates.HtmlFormat.Appendable) = () => apply()
    
    def ref: this.type = this

}
                /*
                    -- GENERATED --
                    DATE: Fri Jan 10 22:31:23 HST 2014
                    SOURCE: /Users/eduardgamiao/Desktop/ICS414/Mepedia/app/views/Joyce.scala.html
                    HASH: 83b34310c3cc1988a691a33bd791759f5edd729a
                    MATRIX: 864->1|893->22|932->24|2118->1179
                    LINES: 29->1|29->1|29->1|53->25
                    -- GENERATED --
                */
            