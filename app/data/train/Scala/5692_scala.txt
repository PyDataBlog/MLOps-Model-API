package org.sisioh.aws4s.dynamodb.extension

import com.amazonaws.services.{ dynamodbv2 => aws }
import org.sisioh.aws4s.dynamodb.Implicits._

case class WriteRequest(underlying: aws.model.WriteRequest) {

  val putRequestOpt = underlying.putRequestOpt

  val deleteRequestOpt = underlying.deleteRequestOpt

}
