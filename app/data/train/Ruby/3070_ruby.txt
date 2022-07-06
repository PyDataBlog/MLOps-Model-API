module Softlayer
  class Generator
    class ModuleFile < ClassFile
      def initialize(name)
        @softlayer_name = name.dup
        @name = Converter.module_name(name)
        raise Exception.new('Not a SoftLayer module') if DataType.object_type(name) == :class
        @autoload = Softlayer::Generator::DataType.autoload_for(@softlayer_name)
      end

      def generate
        content = generate_header
        content << generate_autoload
        content << generate_footer
      end

      def generate_header
        header = ""
        full_name = ""
        step = 0
        total_steps = @name.split("::").size
        @name.split("::").each do |class_name|
          # iteration setup
          full_name << class_name
          step.times { header << "  " }

          # add to header
          object_type = object_type(full_name)
          header << object_type + " " + class_name
          header << "\n"

          # iteration teardown
          step = step + 1
          full_name << "::"
        end
        header
      end
      
      class Exception < Exception; end 
    end
  end
end
