class AlertList
  def initialize(zipcode)
    @response = HTTParty.get("http://api.wunderground.com/api/#{ENV["WUNDERGROUND_KEY"]}/alerts/q/#{zipcode}.json")
  end

  def all_alerts
    @response["alerts"].map {|a| Alert.new(a)}
  end

end


class Alert
  def initialize(json)
    @alert = json
  end

  def alert_type
    @alert["type"]
  end

  def alert_description
    @alert["description"]
  end

  def alert_date
    @alert["date"]
  end

  def alert_expires
    @alert["expires"]
  end

  def alert_message
    @alert["message"]
  end
end


# def test_alerts
#   list = AlertList.new(50301)
#   a = list.all_alerts.first
#   assert_equal "FLO", a.alert_type
#   assert_equal "Flood Warning", a.alert_description
#   assert_equal "3:32 PM CST on February 22, 2016", a.alert_date
#   assert_equal "6:00 AM CST on February 26, 2016", a.alert_expires
#   assert_equal "\n...Flood Warning now in effect until Thursday morning...\n\nThe Flood Warning continues for\n the Des Moines River at Des Moines se 6th St...or from below the \n center street dam...to Runnells.\n* Until Thursday morning.\n* At 3:00 PM Monday the stage was 23.3 feet...or 0.7 feet below \n flood stage.\n* Flood stage is 24.0 feet.\n* No flooding is occurring and minor flooding is forecast.\n* Forecast...rise to flood stage late this evening. Continue rising \n to 24.3 feet...or 0.3 feet above flood stage...midday Tuesday. \n Then begin falling and go below flood stage Thursday morning.\n* Impact...at 24.0 feet...the bike trail is closed east of Water \n Street. Portions of other bike trails are also affected.\n\n\nLat...Lon 4159 9356 4152 9333 4145 9333 4154 9356\n 4159 9366 4159 9356 \n\n\n\n\n", a.alert_message
# end
