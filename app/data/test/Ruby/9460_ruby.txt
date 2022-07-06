require 'spec_helper'

describe "api_keys/index" do
  before(:each) do
    assign(:api_keys, [
      stub_model(ApiKey,
        :user => nil,
        :auth_token => "Auth Token",
        :application => "Application",
        :version => "Version"
      ),
      stub_model(ApiKey,
        :user => nil,
        :auth_token => "Auth Token",
        :application => "Application",
        :version => "Version"
      )
    ])
  end

  it "renders a list of api_keys" do
    render
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "tr>td", :text => nil.to_s, :count => 2
    assert_select "tr>td", :text => "Auth Token".to_s, :count => 2
    assert_select "tr>td", :text => "Application".to_s, :count => 2
    assert_select "tr>td", :text => "Version".to_s, :count => 2
  end
end
