package org.sisioh.aws4s.cfn.model

import com.amazonaws.services.cloudformation.model.ListStacksRequest
import org.sisioh.aws4s.PimpedType

import scala.collection.JavaConverters._

object ListStacksRequestFactory {

  def create(): ListStacksRequest = new ListStacksRequest()

}

class RichListStacksRequest(val underlying: ListStacksRequest)
    extends AnyVal with PimpedType[ListStacksRequest] {

  def nextTokenOpt: Option[String] = Option(underlying.getNextToken)

  def nextTokenOpt_=(value: Option[String]): Unit =
    underlying.setNextToken(value.orNull)

  def withNextTokenOpt(value: Option[String]): ListStacksRequest =
    underlying.withNextToken(value.orNull)

  // ---

  def stackStatusFilters: Seq[String] = underlying.getStackStatusFilters.asScala.toVector

  def stackStatusFilters_=(value: Seq[String]): Unit =
    underlying.setStackStatusFilters(value.asJava)

  def withStackStatusFilters(value: Seq[String]): ListStacksRequest =
    underlying.withStackStatusFilters(value.asJava)

}
