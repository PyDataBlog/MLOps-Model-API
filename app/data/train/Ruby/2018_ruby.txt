module ObserverPattern
  module Weather
    class ForecastDisplay
      include DisplayElement
      include Observer

      attr_accessor :current_pressure, :last_pressure, :weather_data

      def initialize(weather_data)
        self.weather_data = weather_data
        self.weather_data.register_observer(self)

        self.current_pressure = 29.92
      end

      def display
        if current_pressure > last_pressure
          puts_and_return "Forecast: Improving weather on the way!"
        elsif current_pressure == last_pressure
          puts_and_return "Forecast: More of the same"
        elsif current_pressure < last_pressure
          puts_and_return "Forecast: Watch out for cooler, rainy weather"
        end
      end

      def update(temperature, humidity, pressure)
        self.last_pressure = current_pressure
        self.current_pressure = pressure
      end
    end
  end
end
