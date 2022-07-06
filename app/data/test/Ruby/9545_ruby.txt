#
# Cookbook Name:: fateca
# Recipe:: default
#
# Copyright 2014, YOUR_COMPANY_NAME
#
# All rights reserved - Do Not Redistribute
#
application "fateca" do
    path node['fateca']['path']
    owner node['fateca']['owner']
    group node['fateca']['group']
    repository node['fateca']['repository']
    revision node['fateca']['revision']
    enable_submodules node['fateca']['submodules']
    

    # Forces deploy
    if node['fateca']['force_deploy'] == true
        action :force_deploy
    end

    # Enables virtual hosting
    mod_php_apache2 do
        webapp_template "fateca.conf.erb"
    end

    after_restart do
        # Add env variables
        magic_shell_environment 'PATH' do
          value "$PATH:#{new_resource.path}/current/app/Console"
        end

        # Changes cake console permission
        file "#{new_resource.path}/current/app/Console/cake" do
          mode '777'
        end

        # Creates tmp directory
        %w[ /tmp /tmp/cache /tmp/cache/models /tmp/cache/persistent /tmp/cache/views 
            /tmp/logs /tmp/sessions /tmp/test ].each do |path|
            directory "#{new_resource.path}/current/app" + path do
                owner 'vagrant'
                group 'vagrant'
                mode '777'
            end
        end

        template "#{new_resource.path}/current/app/Config/database.php" do
            source "database.php.erb"
            cookbook "fateca"
            owner new_resource.owner
            group new_resource.group
            mode "0664"
        end
    end
end