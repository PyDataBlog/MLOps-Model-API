require 'active_admin'
require 'awesome_nested_fields'
require 'bootstrap-sass'
require 'bootstrap-wysihtml5-rails'
require 'chosen-rails'
require 'jquery-rails'

require 'active_tweaker/version'

module ActiveTweaker

  require 'active_tweaker/engine'
  require 'active_tweaker/tweaks/wysihtml5_input'
  require 'active_tweaker/tweaks/index_table_sort_column'

  ::ActiveAdmin::Views::IndexAsTable.send :include, ActiveTweaker::Tweaks::IndexTableSortColumn
  ::ActiveAdmin::ResourceDSL.send :include, ActiveTweaker::Tweaks::CollectionSortAction

end
