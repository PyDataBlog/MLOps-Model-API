# -*- coding: utf-8 -*-

require File.expand_path(File.join('..', 'spec_helper'), File.dirname(__FILE__))
require 'pukiwikiparser'

describe PukiWikiParser::PukiWikiParser do
  before(:each) do
    @parser = PukiWikiParser::PukiWikiParser.new
  end

  it "should parse hr notation" do
    input = "----"
    html  = "<hr />"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse h2 notation" do
    input = "*example"
    html  = "<h2>example</h2>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse h3 notation" do
    input = "**example"
    html  = "<h3>example</h3>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse h4 notation" do
    input = "***example"
    html  = "<h4>example</h4>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse pre notation" do
    input = " example"
    html  = "<pre><code>example\n</code></pre>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse quote notation" do
    input = "> example"
    html  = "<blockquote><p>\n example\n</p></blockquote>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse ul level 1 notation" do
    input = "-example"
    html  = "<ul>\n<li>example\n</li>\n</ul>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse ul level 2 notation" do
    input = "--example"
    html  = "<ul>\n<ul>\n<li>example\n</li>\n</ul>\n</ul>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse ul level 3 notation" do
    input = "---example"
    html  = "<ul>\n<ul>\n<ul>\n<li>example\n</li>\n</ul>\n</ul>\n</ul>"
    expect(@parser.to_html(input, [])).to eq html
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse ol level 1 notation" do
    input = "+example"
    html  = "<ol>\n<li>example\n</li>\n</ol>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse ol level 2 notation" do
    input = "++example"
    html  = "<ol>\n<ol>\n<li>example\n</li>\n</ol>\n</ol>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse ol level 3 notation" do
    input = "+++example"
    html  = "<ol>\n<ol>\n<ol>\n<li>example\n</li>\n</ol>\n</ol>\n</ol>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse dl notation" do
    input = ":example|foobarbaz"
    html  = "<dl>\n<dt>example</dt>\n<dd>foobarbaz</dd>\n</dl>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse every other inputs as P tag" do
    input = "foo, bar, baz"
    html  = "<p>\nfoo, bar, baz\n</p>"
    expect(@parser.to_html(input, [])).to eq html
  end

  it "should parse line break" do
    input = "example~"
    html  = "<p>\nexample<br />\n</p>"
    expect(@parser.to_html(input, [])).to eq html
  end
end
