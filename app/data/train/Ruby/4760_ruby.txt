require 'rails_helper'

RSpec.describe "rss_sources/new", type: :view do
  before(:each) do
    assign(:rss_source, RssSource.new(
      :name => "MyString",
      :url => "MyString",
      :active => false,
      :published => false
    ))
  end

  it "renders new rss_source form" do
    render

    assert_select "form[action=?][method=?]", rss_sources_path, "post" do

      assert_select "input#rss_source_name[name=?]", "rss_source[name]"

      assert_select "input#rss_source_url[name=?]", "rss_source[url]"

      assert_select "input#rss_source_active[name=?]", "rss_source[active]"

      assert_select "input#rss_source_published[name=?]", "rss_source[published]"
    end
  end
end
