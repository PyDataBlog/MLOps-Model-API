def freq_month(obj):
    if obj is None or obj == []:
        return
    months = {1: 'jan',
              2: 'feb',
              3: 'mar',
              4: 'apr',
              5: 'may',
              6: 'jun',
              7: 'jul',
              8: 'aug',
              9: 'sep',
              10: 'oct',
              11: 'nov',
              12: 'dec',
             }
    frequencies = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

#    for i in range(0, len(obj)):
#        frequencies[ obj[i] -1] += 1

    for i in obj:
        frequencies[ i-1 ] += 1

    print "The following month(s) have a birthday celebration"
    for i in range(0, len(frequencies)):
        if frequencies[i] > 0:
            print str(months[i+1]) + " has " + str(frequencies[i])
    return frequencies

in_array = [3,6,2,7,7,7,]
print freq_month(in_array)
print freq_month([])
