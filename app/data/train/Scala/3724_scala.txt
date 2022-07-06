package org.sisioh.aws4s.cfn.model

import com.amazonaws.services.cloudformation.model.SetStackPolicyRequest
import org.sisioh.aws4s.PimpedType

object SetStackPolicyRequestFactory {

  def create(): SetStackPolicyRequest = new SetStackPolicyRequest()

}

class RichSetStackPolicyRequest(val underlying: SetStackPolicyRequest)
    extends AnyVal with PimpedType[SetStackPolicyRequest] {

  def stackNameOpt: Option[String] = Option(underlying.getStackName)

  def stackNameOpt_=(value: Option[String]): Unit =
    underlying.setStackName(value.orNull)

  def withStackNameOpt(value: Option[String]): SetStackPolicyRequest =
    underlying.withStackName(value.orNull)

  // ---

  def stackPolicyBodyOpt: Option[String] = Option(underlying.getStackPolicyBody)

  def stackPolicyBodyOpt_=(value: Option[String]): Unit =
    underlying.setStackPolicyBody(value.orNull)

  def withStackPolicyBodyOpt(value: Option[String]): SetStackPolicyRequest =
    underlying.withStackPolicyBody(value.orNull)

  // ---

  def stackPolicyURLOpt: Option[String] = Option(underlying.getStackPolicyURL)

  def stackPolicyURLOpt_=(value: Option[String]): Unit =
    underlying.setStackPolicyURL(value.orNull)

  def withStackPolicyURLOpt(value: Option[String]): SetStackPolicyRequest =
    underlying.withStackPolicyURL(value.orNull)

}
