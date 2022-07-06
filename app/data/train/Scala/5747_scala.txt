package organizing_data.replace_type_code_with_class.after

class Person(var bloodGroup: BloodGroup ){
  def bloodGroupCode = bloodGroup.code
}

