class BusStopController < ApplicationController
	protect_from_forgery with: :null_session, if: Proc.new { |c| c.request.format == 'application/json' }

  def api
    obj = params["data"]
    p params["data"]
    address = obj.split("','")[0]
    bus_num = obj.split("','")[1]
    bus_num = bus_num[0..-2]
    response = Geokit::Geocoders::MultiGeocoder.do_geocode(address)
    respond_to do |format|
      format.html { }
      format.json  { render :json => BusStop.find_closest_by_coord_and_bus(response.longitude,response.latitude,bus_num) }
      format.xml  { }
    end
  end
end
