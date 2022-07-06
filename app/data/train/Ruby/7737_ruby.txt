###############################################################################
# Cookbook Name:: libsodium
# Recipe:: default
# Copyright 2016, Joshua C. Burt
###############################################################################

###############################################################################
# Ensure we have a sane build environment
###############################################################################
include_recipe 'build-essential'

###############################################################################
# Clone our remote repository
###############################################################################
git node['libsodium']['src'] do
  repository node['libsodium']['src_url']
  revision node['libsodium']['version']
  action :sync
end

###############################################################################
# Execute *nix still compile workflow
###############################################################################
libsodium_builder 'libsodium compile and install'

###############################################################################
# Configure ourselves in the library cache
###############################################################################
template '/etc/ld.so.conf.d/libsodium.conf' do
  group 'root'
  mode '0444'
  owner 'root'
  notifies :run, 'execute[ldconfig]', :immediately
end

###############################################################################
# Update library cache
###############################################################################
execute 'ldconfig' do
  action :nothing
  command 'ldconfig'
end
