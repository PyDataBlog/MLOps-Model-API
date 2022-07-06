require "spec_helper"

describe ArtsGalleryApi::Gallery do
  let(:gallery) { {"id" => 1,"name" => "Hayward Gallery","description" => "Hayward Gallery"} }
  let(:galleries) do 
    {"galleries" => [{ "name" => "Hayward Gallery", "url" => "/galleries/1",
      "exhibitions" => "/galleries/1/exhibitions" }] }
  end
  let(:exhibition) { {"id" => 1,"name" => "Futurism Then And Now", "entry_fee" => "£15.00"} }
  let(:exhibitions) do
   {"exhibitions" => [{"name" => "Futurism Then And Now","gallery_name" => "Hayward Gallery",
    "url" => "/exhibitions/1","gallery_url" => "/galleries/1"}]}
  end

  describe "#all" do
    it "retrieves all galleries" do
      response = double(parsed_response: galleries )
      HTTParty.should_receive(:get).and_return(response)
      subject.all
      subject.response.should eq(galleries)
    end
  end

  describe "#retrieve_a_gallery" do
    it "retrieves one gallery" do
      response = double(parsed_response: gallery  )
      HTTParty.should_receive(:get).and_return(response)
      subject.retrieve_a_gallery(1)
      subject.response.should eq( gallery ) 
    end
  end
  describe "#get_gallery_exhibitions" do
    it "gets exhibitions at galleries" do
      response = double(parsed_response: exhibitions  )
      HTTParty.should_receive(:get).and_return(response)
      subject.get_gallery_exhibitions(1)
      subject.response.should eq( exhibitions ) 
   end
  end
    describe "#get_exhibition" do
    it "gets an exhibition" do
      response = double(parsed_response: exhibition  )
      HTTParty.should_receive(:get).and_return(response)
      subject.get_exhibition(1)
      subject.response.should eq( exhibition ) 
   end
  end

end