require 'spec_helper'

describe "shops/index" do
  before(:each) do
    assign(:shops, [
      stub_model(Shop,
        :number => 1,
        :name => "Name",
        :town => 2,
        :street => 3,
        :bldg => "Bldg",
        :housing => "Housing",
        :floor => "Floor",
        :id_hard => 4,
        :id_stock => 5,
        :business_hours => "Business Hours",
        :id_provider => 6,
        :phone => "Phone"
      ),
      stub_model(Shop,
        :number => 1,
        :name => "Name",
        :town => 2,
        :street => 3,
        :bldg => "Bldg",
        :housing => "Housing",
        :floor => "Floor",
        :id_hard => 4,
        :id_stock => 5,
        :business_hours => "Business Hours",
        :id_provider => 6,
        :phone => "Phone"
      )
    ])
  end

  it "renders a list of shops" do
    render
    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "tr>td", :text => 1.to_s, :count => 2
    assert_select "tr>td", :text => "Name".to_s, :count => 2
    assert_select "tr>td", :text => 2.to_s, :count => 2
    assert_select "tr>td", :text => 3.to_s, :count => 2
    assert_select "tr>td", :text => "Bldg".to_s, :count => 2
    assert_select "tr>td", :text => "Housing".to_s, :count => 2
    assert_select "tr>td", :text => "Floor".to_s, :count => 2
    assert_select "tr>td", :text => 4.to_s, :count => 2
    assert_select "tr>td", :text => 5.to_s, :count => 2
    assert_select "tr>td", :text => "Business Hours".to_s, :count => 2
    assert_select "tr>td", :text => 6.to_s, :count => 2
    assert_select "tr>td", :text => "Phone".to_s, :count => 2
  end
end
