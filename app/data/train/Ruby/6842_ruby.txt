require 'spec_helper'

describe package('bash') do
  it { should be_installed }
end

describe package('bash-completion') do
  it { should be_installed }
end

describe file('/root/.bash_profile') do
  its(:content) { should match /export TMOUT=60/ }
end
