package org.sisioh.aws4s.cfn.model

import com.amazonaws.services.cloudformation.model.DeleteStackRequest
import org.sisioh.aws4s.PimpedType

object DeleteStackRequestFactory {

  def create(): DeleteStackRequest = new DeleteStackRequest()

}

class RichDeleteStackRequest(val underlying: DeleteStackRequest)
    extends AnyVal with PimpedType[DeleteStackRequest] {

  def stackNameOpt: Option[String] = Option(underlying.getStackName)

  def stackNameOpt_=(value: Option[String]): Unit =
    underlying.setStackName(value.orNull)

  def withStackNameOpt(value: Option[String]): DeleteStackRequest =
    underlying.withStackName(value.orNull)

}
