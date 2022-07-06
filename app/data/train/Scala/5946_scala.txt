package org.sisioh.aws4s.cfn.model

import com.amazonaws.services.cloudformation.model.EstimateTemplateCostResult
import org.sisioh.aws4s.PimpedType

object EstimateTemplateCostResultFactory {

  def create(): EstimateTemplateCostResult = new EstimateTemplateCostResult()

}

class RichEstimateTemplateCostResult(val underlying: EstimateTemplateCostResult)
    extends AnyVal with PimpedType[EstimateTemplateCostResult] {

  def urlOpt: Option[String] = Option(underlying.getUrl)

  def urlOpt_=(value: Option[String]): Unit =
    underlying.setUrl(value.orNull)

  def witUrlOpt(value: Option[String]): EstimateTemplateCostResult =
    underlying.withUrl(value.orNull)

}
