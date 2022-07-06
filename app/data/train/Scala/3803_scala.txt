
package views.html.feedback

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
object feedbackIndex extends BaseScalaTemplate[play.api.templates.Html,Format[play.api.templates.Html]](play.api.templates.HtmlFormat) with play.api.templates.Template3[com.mnt.core.helper.SearchContext,dto.MenuBar,models.user.User,play.api.templates.Html] {

    /**/
    def apply/*1.2*/(context:com.mnt.core.helper.SearchContext,_menuContext: dto.MenuBar,user : models.user.User):play.api.templates.Html = {
        _display_ {

Seq[Any](format.raw/*1.95*/("""
"""),_display_(Seq[Any](/*2.2*/main("Feedback")/*2.18*/ {_display_(Seq[Any](format.raw/*2.20*/("""
    """),_display_(Seq[Any](/*3.6*/menuContext(_menuContext,user))),format.raw/*3.36*/("""
    """),_display_(Seq[Any](/*4.6*/searchContext(context,null,"add"))),format.raw/*4.39*/("""
""")))})),format.raw/*5.2*/("""
"""))}
    }
    
    def render(context:com.mnt.core.helper.SearchContext,_menuContext:dto.MenuBar,user:models.user.User): play.api.templates.Html = apply(context,_menuContext,user)
    
    def f:((com.mnt.core.helper.SearchContext,dto.MenuBar,models.user.User) => play.api.templates.Html) = (context,_menuContext,user) => apply(context,_menuContext,user)
    
    def ref: this.type = this

}
                /*
                    -- GENERATED --
                    DATE: Mon Apr 21 14:16:21 EDT 2014
                    SOURCE: /Users/Secret/Technology/play-2.1.0/Time_Trotter/app/views/feedback/feedbackIndex.scala.html
                    HASH: 35b74d239bcf12ca304c551c36e26bfe25fa7a51
                    MATRIX: 796->1|966->94|1003->97|1027->113|1066->115|1107->122|1158->152|1199->159|1253->192|1286->195
                    LINES: 26->1|29->1|30->2|30->2|30->2|31->3|31->3|32->4|32->4|33->5
                    -- GENERATED --
                */
            