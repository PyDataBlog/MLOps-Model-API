# encoding: utf-8

class GetFreeTimeResponseObjMapper
  include XmlSchemaMapper
  schema File.expand_path('../../vendor/ezags-protocols/eZAGS/public/OrderService.xsd', File.dirname(__FILE__))
  type 'GetFreeTimeResponseObj'

  # @return [ListOfFreeDatesMapper]
  # minOccurs: 1, maxOccurs: 1
  attr_accessor :return

end
