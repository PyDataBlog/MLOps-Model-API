package org.sisioh.aws4s.cfn.model

import com.amazonaws.services.cloudformation.model.{ CreateStackRequest, Parameter, Tag }
import org.sisioh.aws4s.PimpedType

import scala.collection.JavaConverters._

object CreateStackRequestFactory {

  def create(): CreateStackRequest = new CreateStackRequest

}

class RichCreateStackRequest(val underlying: CreateStackRequest)
    extends AnyVal with PimpedType[CreateStackRequest] {

  def stackNameOpt: Option[String] = Option(underlying.getStackName)

  def stackNameOpt_=(value: Option[String]): Unit =
    underlying.setStackName(value.orNull)

  def withStackNameOpt(value: Option[String]): CreateStackRequest =
    underlying.withStackName(value.orNull)

  // ---

  def templateBodyOpt: Option[String] = Option(underlying.getTemplateBody)

  def templateBodyOpt_=(value: Option[String]): Unit =
    underlying.setTemplateBody(value.orNull)

  def withTemplateBodyOpt(value: Option[String]): CreateStackRequest =
    underlying.withTemplateBody(value.orNull)

  // ---

  def templateURLOpt: Option[String] = Option(underlying.getTemplateURL)

  def templateURLOpt_=(value: Option[String]): Unit =
    underlying.setTemplateURL(value.orNull)

  def withTemplateURLOpt(value: Option[String]): CreateStackRequest =
    underlying.withTemplateURL(value.orNull)

  // ---

  def parameters: Seq[Parameter] = underlying.getParameters.asScala.toVector

  def parameters_=(value: Seq[Parameter]): Unit =
    underlying.setParameters(value.asJava)

  def withParameters(value: Seq[Parameter]): CreateStackRequest =
    underlying.withParameters(value.asJava)

  // ---

  def disableRollbackOpt: Option[Boolean] = Option(underlying.getDisableRollback)

  def disableRollbackOpt(value: Option[Boolean]): Unit =
    underlying.setDisableRollback(value.map(_.asInstanceOf[java.lang.Boolean]).orNull)

  def withDisableRollbackOpt(value: Option[Boolean]): CreateStackRequest =
    underlying.withDisableRollback(value.map(_.asInstanceOf[java.lang.Boolean]).orNull)

  // ---

  def timeoutInMinutesOpt: Option[Int] = Option(underlying.getTimeoutInMinutes)

  def timeoutInMinutesOpt_=(value: Option[Int]): Unit =
    underlying.setTimeoutInMinutes(value.map(_.asInstanceOf[java.lang.Integer]).orNull)

  def timeoutInMinutesOpt(value: Option[Int]): CreateStackRequest =
    underlying.withTimeoutInMinutes(value.map(_.asInstanceOf[java.lang.Integer]).orNull)

  // ---

  def notificationARNs: Seq[String] = underlying.getNotificationARNs.asScala.toVector

  def notificationARNs_=(value: Seq[String]): Unit =
    underlying.setNotificationARNs(value.asJava)

  def withNotificationARNs(value: Seq[String]): CreateStackRequest =
    underlying.withNotificationARNs(value.asJava)

  // ---

  def capabilities: Seq[String] = underlying.getCapabilities.asScala.toVector

  def capabilities_=(value: Seq[String]): Unit =
    underlying.setCapabilities(value.asJava)

  def withCapabilities(value: Seq[String]): CreateStackRequest =
    underlying.withCapabilities(value.asJava)

  // ---

  def onFailureOpt: Option[String] = Option(underlying.getOnFailure)

  def onFailureOpt_=(value: Option[String]): Unit =
    underlying.setOnFailure(value.orNull)

  def withOnFailureOpt(value: Option[String]): CreateStackRequest =
    underlying.withOnFailure(value.orNull)

  // ---

  def stackPolicyBodyOpt: Option[String] = Option(underlying.getStackPolicyBody)

  def stackPolicyBodyOpt_=(value: Option[String]): Unit =
    underlying.setStackPolicyBody(value.orNull)

  def withStackPolicyBodyOpt(value: Option[String]): CreateStackRequest =
    underlying.withStackPolicyBody(value.orNull)

  // ---

  def stackPolicyURLOpt: Option[String] = Option(underlying.getStackPolicyURL)

  def stackPolicyURLOpt_=(value: Option[String]): Unit =
    underlying.setStackPolicyURL(value.orNull)

  def withStackPolicyURLOpt(value: Option[String]): CreateStackRequest =
    underlying.withStackPolicyURL(value.orNull)

  // ---

  def tags: Seq[Tag] = underlying.getTags.asScala.toVector

  def tags_=(value: Seq[Tag]): Unit =
    underlying.setTags(tags.asJava)

  def withTags(value: Seq[Tag]): CreateStackRequest =
    underlying.withTags(value.asJava)

}
