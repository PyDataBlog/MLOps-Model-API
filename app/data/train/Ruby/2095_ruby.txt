require 'test_helper'

class PandaControllerTest < ActionDispatch::IntegrationTest
  test "should get play" do
    get panda_play_url
    assert_response :success
  end

end
