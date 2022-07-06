module Retriable
  def retriable(times=3, &block)
    begin
      yield if block_given?
    rescue => e
      retry if (times -= 1) > 0
      raise e
    end
  end
end

class Object
  include Retriable
end
