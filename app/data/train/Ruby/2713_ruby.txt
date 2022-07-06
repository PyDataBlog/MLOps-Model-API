require_relative "../spec_helper"

describe "TimeParser" do

  it "is able to extract time from text (format 1)" do
    response = CommonRegex::TimeParser.parse("Lets meet between 4:00 pm to 5:00 pm")
    expect(response.length).to eq(2)
    expect(response).to include("4:00 pm")
    expect(response).to include("5:00 pm")
  end

  it "is able to extract time from text (format 2)" do
    response = CommonRegex::TimeParser.parse("Lets meet between 1600 hrs to 1700 hrs")
    expect(response.length).to eq(2)
    expect(response).to include("1600 hrs")
    expect(response).to include("1700 hrs")
  end

  it "is able to extract time from text (format 3)" do
    response = CommonRegex::TimeParser.parse("Lets meet between 4:00 pm to 1700 hrs")
    expect(response.length).to eq(2)
    expect(response).to include("4:00 pm")
    expect(response).to include("1700 hrs")
  end

  it "is able to extract time from text (format 4)" do
    response = CommonRegex::TimeParser.parse("Lets meet between 4:00pm to 1700hrs")
    expect(response.length).to eq(2)
    expect(response).to include("4:00pm")
    expect(response).to include("1700hrs")
  end

end