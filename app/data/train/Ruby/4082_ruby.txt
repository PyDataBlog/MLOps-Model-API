require (ENV['OPERATOR_HOME'] + '/vagrant/lib/common/module/paths.rb')
include CommonPaths

eval File.read( COMMON + '/pre/0_synced.rb' )

if File.file?("synced.rb")
  eval File.read("synced.rb")
end

eval File.read( COMMON + '/pre/1_init.rb' )
eval File.read( COMMON + '/pre/2_install.rb' )
eval File.read( COMMON + '/pre/3_setup.rb' )
eval File.read( COMMON + '/pre/4_system.rb' )

if File.file?("provision.rb")
  eval File.read("provision.rb")
end

eval File.read( COMMON + '/post/cleanup.rb' )
