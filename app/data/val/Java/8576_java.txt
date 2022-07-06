// Complex API code
public class ComplexPricingStrategy {
	int calculateAirlinePricing(String source, String destination) {
		return source.length() + destination.length();
	}

  int calculateProprtyValuation(String location) {
    return (location.length() * 150 % 40 + 12345 - 50) >> 1;
  }

  int calculateOilValuation(String location) {
  	int arbitrary_oil_weight = 87;
  	int complex_oil_expression = (arbitrary_oil_weight * 13) * 321;
  	return (complex_oil_expression * location.length()) << 2;
  }
}
