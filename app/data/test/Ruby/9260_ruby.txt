#
# Cookbook Name:: setup
# Recipe:: repos
#

case node['platform_family']
  when 'debian'
    # Load the default recipe for the cookbook apt
    include_recipe "apt"

    # Load base pacakge repositories for our node
    # This uses the resource defined in the apt cookbook.
    node[:setup][:repositories].each do |key, repository|
        apt_repository repository[:repo_name] do
            uri repository[:uri]
            distribution repository[:distribution]
            components repository[:components]
            key repository[:key]
            keyserver "pgpkeys.mit.edu"
            action :add
        end
    end

    # apt-get update manually
    execute 'apt-get-update' do
      command 'apt-get update'
      ignore_failure true
      only_if { apt_installed? }
      not_if { ::File.exists?('/var/lib/apt/periodic/update-success-stamp') }
    end
  else
    Chef::Log.error "This recipe is not supported on your OS"
    return
end