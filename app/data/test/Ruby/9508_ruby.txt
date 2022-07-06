Ohai.plugin(:Patching) do
  require 'mixlib/shellout'
  provides 'patching','patching/missing'

  collect_data(:default) do
  	patching Mash.new
  	patching[:missing] = Mash.new

    patches = Mixlib::ShellOut.new("yum check-update --quiet|grep -v '^$'")
    patches.run_command
 
    patches.stdout.each_line do |line|
    	package, version, repo = parse_line(line)
    	patching[:missing][package] = Mash.new({ :name => package, :version => version, :repo => repo } )
    end
  end

  def parse_line(line)
  	line=line.gsub(/\s+/,',')
	package, version, repo = line.split(/,/,3)
  	return package, version, repo
  end
end