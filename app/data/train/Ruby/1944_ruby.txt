# Data com mês por extenso. Construa uma função que receba uma data
# no formato DD/MM/AAAA e devolva uma string no formato D de mesPorExtenso de AAAA. 
# Opcionalmente, valide a data e retorne NULL caso a data seja inválida.

require 'test/unit'
require_relative 'date_format'

class TestDateFormat < Test::Unit::TestCase

  def test_to_br_convertion
    @date_format = DateFormat.new('12/03/1991')
    assert_equal(@date_format.to_br, '12 de Mar de 1991')
  end

  def test_to_br_detects_invalid_date_format
    @date_format = DateFormat.new('1991/03/12')
    assert_equal(@date_format.to_br, 'Invalid Date Format')
  end
end