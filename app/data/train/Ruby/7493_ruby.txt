module Resultable
  class Result

    module Status
      SUCCESSFUL = 'successful'
      ERROR = 'error'
    end

    attr_accessor :status, :response

    def initialize
      self.error!
    end

    def successful!
      self.status = Status::SUCCESSFUL
      self
    end

    def successful?
      self.status == Status::SUCCESSFUL
    end

    def error!
      self.status = Status::ERROR
      self
    end

    def error?
      self.status == Status::ERROR
    end
  end
end
