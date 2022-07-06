print 'Immettere un raggio: '
raggio_stringa = gets
raggio = raggio_stringa.to_i
area = raggio * raggio * Math::PI
puts "L'area del cerchio di raggio #{raggio} e' #{area}"

