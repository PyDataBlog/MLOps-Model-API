#
# Cookbook Name:: dovecot
# Recipe:: default
#
# Copyright 2014, YOUR_COMPANY_NAME
#
# All rights reserved - Do Not Redistribute
#

%w{dovecot-core dovecot-imapd dovecot-pop3d}.each do |p|
  package "#{p}" do
    action :install
  end
end

cookbook_file '/etc/dovecot/dovecot.conf' do
  source 'dovecot.conf'
  mode 0644
end
