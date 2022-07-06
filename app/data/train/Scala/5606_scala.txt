package org.sisioh.aws4s.dynamodb.model

import com.amazonaws.services.dynamodbv2.model.{ AttributeValue, ItemCollectionMetrics }
import org.sisioh.aws4s.PimpedType

import scala.collection.JavaConverters._

object ItemCollectionMetricsFactory {

  def create(): ItemCollectionMetrics = new ItemCollectionMetrics()

}

class RichItemCollectionMetrics(val underlying: ItemCollectionMetrics) extends AnyVal with PimpedType[ItemCollectionMetrics] {

  // ---

  def itemCollectionKeyOpt: Option[Map[String, AttributeValue]] = Option(underlying.getItemCollectionKey).map(_.asScala.toMap)

  def itemCollectionKeyOpt_=(value: Option[Map[String, AttributeValue]]): Unit = underlying.setItemCollectionKey(value.map(_.asJava).orNull)

  def withItemCollectionKeyOpt(value: Option[Map[String, AttributeValue]]): ItemCollectionMetrics = underlying.withItemCollectionKey(value.map(_.asJava).orNull)

  // ---

  def sizeEstimateRangeGBOpt: Option[Seq[Double]] = Option(underlying.getSizeEstimateRangeGB).map(_.asScala.map(_.asInstanceOf[Double]))

  def sizeEstimateRangeGBOpt_=(value: Option[Seq[Double]]): Unit = underlying.setSizeEstimateRangeGB(value.map(_.map(_.asInstanceOf[java.lang.Double]).asJava).orNull)

  def withSizeEstimateRangeGBOpt(value: Option[Seq[Double]]): ItemCollectionMetrics = underlying.withSizeEstimateRangeGB(value.map(_.map(_.asInstanceOf[java.lang.Double]).asJava).orNull)

  // ---

}
