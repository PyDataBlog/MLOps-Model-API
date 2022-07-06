require 'rails/generators/active_record'

module OpenId
  module ActiveRecord
    module Store
      class StoreGenerator < ::ActiveRecord::Generators::Base
        desc "Generates the migrations for an open-id server"

        def self.source_root
          @source_root ||= File.expand_path('../templates', __FILE__)
        end

        def generate_migration
          migration_template "#{migration_name}.rb.erb", "db/migrate/#{migration_file_name}"
        end

        def migration_name
          "create_open_id_tables"
        end

        def migration_file_name
          "#{migration_name}.rb"
        end

        def migration_class_name
          migration_name.camelize
        end
      end
    end
  end
end