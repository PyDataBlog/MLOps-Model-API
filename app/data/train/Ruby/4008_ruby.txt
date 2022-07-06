class EchoController < ApplicationController
  before_filter :deep_snake_case_params!

  def echo
    render json: params
  end
end
