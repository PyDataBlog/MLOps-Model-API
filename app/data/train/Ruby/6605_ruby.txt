require "tunneld/version"

module Tunneld
  def self.start
  	system "thin start -R #{File.expand_path('../config.ru', __FILE__)}"
  end
end
