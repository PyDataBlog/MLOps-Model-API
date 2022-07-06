require 'spec_helper'

describe "banners/new" do
  before(:each) do
    assign(:banner, stub_model(Banner,
      :archivo => "MyString",
      :url => "MyString",
      :cliente => nil
    ).as_new_record)
  end

  it "renders new banner form" do
    render

    # Run the generator again with the --webrat flag if you want to use webrat matchers
    assert_select "form[action=?][method=?]", banners_path, "post" do
      assert_select "input#banner_archivo[name=?]", "banner[archivo]"
      assert_select "input#banner_url[name=?]", "banner[url]"
      assert_select "input#banner_cliente[name=?]", "banner[cliente]"
    end
  end
end
