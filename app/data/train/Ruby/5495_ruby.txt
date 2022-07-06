require 'test_helper'

class SecretLanguagesControllerTest < ActionDispatch::IntegrationTest
  setup do
    @secret_language = secret_languages(:one)
  end

  test "should get index" do
    get secret_languages_url
    assert_response :success
  end

  test "should get new" do
    get new_secret_language_url
    assert_response :success
  end

  test "should create secret_language" do
    assert_difference('SecretLanguage.count') do
      post secret_languages_url, params: { secret_language: { content: @secret_language.content } }
    end

    assert_redirected_to secret_language_url(SecretLanguage.last)
  end

  test "should show secret_language" do
    get secret_language_url(@secret_language)
    assert_response :success
  end

  test "should get edit" do
    get edit_secret_language_url(@secret_language)
    assert_response :success
  end

  test "should update secret_language" do
    patch secret_language_url(@secret_language), params: { secret_language: { content: @secret_language.content } }
    assert_redirected_to secret_language_url(@secret_language)
  end

  test "should destroy secret_language" do
    assert_difference('SecretLanguage.count', -1) do
      delete secret_language_url(@secret_language)
    end

    assert_redirected_to secret_languages_url
  end
end
