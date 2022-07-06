function alphavalue(c)
	int(c) - int('A') +1
end

function totalalphavalue(s)
	mapreduce(alphavalue, +, collect(s))
end

function main()
	filepath = pwd() * "/p022_names.txt"
	text = open(readall, filepath)
	text = replace(text, ['"','\\'], "")
	names = split(text,',')

	sort!(names)

	namevalues = map(totalalphavalue, names)
	sum(namevalues .* (1:length(namevalues)))

end

main()
