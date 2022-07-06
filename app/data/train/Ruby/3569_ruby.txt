class CurrenciesController
  def index
    if Currency.count > 0
      currencies = Currency.all # All of the currencies in an array
      currencies.each_with_index do |currency, index|
        say("#{index + 1}. #{currency_X.country_name}")
      end
    else
      say("No currency records found. Add one.\n")
    end
  end
end