require "awesome_print"
require "active_record"
require "active_console/version"
require "active_console/cols"
require "active_console/agrep"
require "active_console/rels"

Dir[File.dirname(__FILE__) + "/active_console/*.rb"].each { |file| require file }

module ActiveConsole
  include Cols
  include Agrep
  include Alias
  include Similar
  include Rels
end
