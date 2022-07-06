using Match

function isLeapYear(y)
	y%4==0 && !(y%100==0 && !(y%400==0))
	# if !(y%4==0)
	# 	false
	# elseif !(y%100==0)
	# 	true
	# elseif !(y%400==0)
	# 	false
	# else
	# 	true
	# end
end

function nextday(currentdate)
	y = currentdate["year"]
	leapy = isLeapYear(y)
	m = currentdate["month"]
	nDaysInMonth = @match m begin
        1  => 31
        2  => leapy ? 29 : 28
        3  => 31
        4  => 30
        5  => 31
        6  => 30
        7  => 31
        8  => 31
        9  => 30
        10 => 31
        11 => 30
        12 => 31
        _  => error("bug")
    end
    n = currentdate["number"]
    isNewMonth = (n == nDaysInMonth)
    newn = isNewMonth ? 1 : n+1
    newm = isNewMonth ? (m==12 ? 1 : m+1) : m
    newy = (isNewMonth && m==12) ? y+1 : y

	d = currentdate["day"]
    newd = ((d+1 -1) % 7) +1
    #println("$d $n $m $y  $leapy $nDaysInMonth $isNewMonth")
    date = ["year"=>newy,"month"=>newm,"number"=>newn,"day"=>newd]
    #println("$date ")
    return date
end

function nextdayuntil(startdate, enddate, countpred)
	c = 0

	date = startdate
	while date["year"] < enddate["year"] ||
			date["month"] != enddate["month"] ||
			date["number"] != enddate["number"]
			
		if countpred(date)
			c = c+1
		end
		date = nextday(date)
	end
	return (c, date)
end

function main()
	# from http://kalender-365.de/leap-years.php
	listLeapYear = [1804,1808,1812,1816,1820,1824,1828,1832,1836,1840,1844,1848,1852,1856,1860,1864,1868,1872,1876,1880,1884,1888,1892,1896,
	1904,1908,1912,1916,1920,1924,1928,1932,1936,1940,1944,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,
	2004,2008,2012,2016,2020,2024,2028,2032,2036,2040,2044,2048,2052,2056,2060,2064,2068,2072,2076,2080,2084,2088,2092,2096]
	assert(all(map(isLeapYear, filter(x -> in(x,listLeapYear), 1800:2100))))
	assert(!(any(map(isLeapYear, filter(x -> !in(x,listLeapYear), 1800:2100)))))
	
	initdate  = ["year"=>1900,"month"=>1,"number"=>1,"day"=>1]
	startdate = ["year"=>1901,"month"=>1,"number"=>1]
	# augment startdate with day information
	(_,startdate) = nextdayuntil(initdate, startdate, x -> false)
	
	enddate   = ["year"=>2000,"month"=>12,"number"=>31]
	nextdayuntil(startdate, enddate, x -> x["number"] == 1 && x["day"] == 7)
end

main()
