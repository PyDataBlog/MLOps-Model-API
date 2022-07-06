module Matrix
  class MainMatrix < Matrix::Base
    
    attr_reader :result
  
    def accumulate_by sub_matrix
      current_value = 0
      sub_matrix.each do |element|
        x = element.x_index
        y = element.y_index
        @rows[y] ||= []
        main_element_value = @rows[y][x].to_i
        current_value += main_element_value
        set x: x, y: y, value: current_value
      end
      @result = current_value
      @result_matrix
    end
    
    def set options={}
      @result_matrix ||= @rows.clone
      @result_matrix[options[:y]][options[:x]] = options[:value]
    end
  
  end
end