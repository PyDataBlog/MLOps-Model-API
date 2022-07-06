package org.sisioh.aws4s.s3.model

import com.amazonaws.services.s3.model.DeleteObjectRequest
import org.sisioh.aws4s.PimpedType

object DeleteObjectRequestFactory {

  def create(bucketName: String, key: String): DeleteObjectRequest =
    new DeleteObjectRequest(bucketName, key)

}

class RichDeleteObjectRequest(val underlying: DeleteObjectRequest)
    extends AnyVal with PimpedType[DeleteObjectRequest] {

  def bucketName = underlying.getBucketName

  def bucketName_=(value: String): Unit =
    underlying.setBucketName(value)

  def key = underlying.getKey

  def key_=(value: String): Unit =
    underlying.setKey(value)

}
