require 'rails_helper'

RSpec.describe "customers/index", :type => :view do
  before(:each) do
    assign(:customers, [
      Customer.create!(
        :name => "Name",
        :kana => "Kana",
        :email => "Email",
        :sex => 1,
        :age => 2,
        :marriage => 3,
        :pref => "Pref",
        :carrier => "Carrier",
        :curry => "Curry"
      ),
      Customer.create!(
        :name => "Name",
        :kana => "Kana",
        :email => "Email",
        :sex => 1,
        :age => 2,
        :marriage => 3,
        :pref => "Pref",
        :carrier => "Carrier",
        :curry => "Curry"
      )
    ])
  end

  it "renders a list of customers" do
    render
    assert_select "tr>td", :text => "Name".to_s, :count => 2
    assert_select "tr>td", :text => "Kana".to_s, :count => 2
    assert_select "tr>td", :text => "Email".to_s, :count => 2
    assert_select "tr>td", :text => 1.to_s, :count => 2
    assert_select "tr>td", :text => 2.to_s, :count => 2
    assert_select "tr>td", :text => 3.to_s, :count => 2
    assert_select "tr>td", :text => "Pref".to_s, :count => 2
    assert_select "tr>td", :text => "Carrier".to_s, :count => 2
    assert_select "tr>td", :text => "Curry".to_s, :count => 2
  end
end
