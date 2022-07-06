require 'spec_helper'

describe 'sublime_text_3' do
  it do
    should contain_package('SublimeText3').with({
      :provider => 'appdmg',
      :source   => 'http://c758482.r82.cf2.rackcdn.com/Sublime%20Text%20Build%203047.dmg',
    })
  end
end