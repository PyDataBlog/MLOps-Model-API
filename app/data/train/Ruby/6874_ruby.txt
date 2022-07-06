#
# Cookbook Name:: galaxy-toolshed
# Recipe:: default
#
# Copyright 2014, RIKEN ACCC
#
# All rights reserved - Do Not Redistribute
#
user "galaxy" do
    username node[:galaxy-toolshed][:user]
    home     node[:galaxy-toolshed][:home]
    shell    node[:galaxy-toolshed][:shell]
    password node[:galaxy-toolshed][:password]

    supports :manage_home => true
    action   :create
end
# set directory owner and permission mainly for shared file system
directory node[:galaxy-toolshed][:home] do
    owner node[:galaxy-toolshed][:user]
    group      node[:galaxy-toolshed][:group]
    mode '0755'
end

include_recipe "mercurial"
mercurial node[:galaxy-toolshed][:path] do
    repository node[:galaxy-toolshed][:repository]
    owner      node[:galaxy-toolshed][:user]
    group      node[:galaxy-toolshed][:group]
    reference  node[:galaxy-toolshed][:reference]

    action     :clone
end

# galaxy main directory
directory node[:galaxy-toolshed][:path] do
    owner node[:galaxy-toolshed][:user]
    group      node[:galaxy-toolshed][:group]
    mode '0755'
end

include_recipe "python"
virtualenv_home  = "#{node[:galaxy-toolshed][:path]}/.venv"
user_name        = node[:galaxy-toolshed][:user]

# install
python_pip "virtualenv" do
    action :install
end
python_virtualenv virtualenv_home do
  action :create
  owner node[:galaxy-toolshed][:user]
  group node[:galaxy-toolshed][:group]
end

template "#{node[:galaxy-toolshed][:path]}/#{node[:galaxy-toolshed][:config]}" do
    owner      node[:galaxy-toolshed][:user]
    group      node[:galaxy-toolshed][:group]
    mode       "0644"
    source     "galaxy-toolshed.conf.erb"

    action     :create
end

unless node[:galaxy-toolshed][:initfile].nil?
    template node[:galaxy-toolshed][:initfile] do
        owner      "root"
        group      "root"
        mode       "0755"
        source     "galaxy-toolshed.init.erb"

        action     :create
    end
    bash "add_galaxy-toolshed_service" do
        code "chkconfig --add galaxy-toolshed"
        user root

        action :run
    end
    service "galaxy-toolshed" do
        action [:enable, :start]
        supports :status => true, :restart => true, :reload => true
    end
end

