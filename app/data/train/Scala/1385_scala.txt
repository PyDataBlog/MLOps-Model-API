package io.swagger.client.model

import io.swagger.client.core.ApiModel
import org.joda.time.DateTime


case class Credential (
  /* connector_id */
  connectorId: Int,
  /* attr_key */
  attrKey: Option[String],
  /* attr_value */
  attrValue: Option[String],
  /* created_at */
  createdAt: Option[DateTime],
  /* updated_at */
  updatedAt: Option[DateTime])
   extends ApiModel


