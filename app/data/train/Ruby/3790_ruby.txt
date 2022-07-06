require 'test_helper'

class BricksControllerTest < ActionController::TestCase
  setup do
    @brick = bricks(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:bricks)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create brick" do
    assert_difference('Brick.count') do
      post :create, brick: { description: @brick.description, name: @brick.name, price: @brick.price, set_number: @brick.set_number }
    end

    assert_redirected_to brick_path(assigns(:brick))
  end

  test "should show brick" do
    get :show, id: @brick
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @brick
    assert_response :success
  end

  test "should update brick" do
    patch :update, id: @brick, brick: { description: @brick.description, name: @brick.name, price: @brick.price, set_number: @brick.set_number }
    assert_redirected_to brick_path(assigns(:brick))
  end

  test "should destroy brick" do
    assert_difference('Brick.count', -1) do
      delete :destroy, id: @brick
    end

    assert_redirected_to bricks_path
  end
end
