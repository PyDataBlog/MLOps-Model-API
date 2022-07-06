require 'socket'

'''
The API to this is very simple

GET / - returns all keys
GET /key - returns value of key "key"
GET /key/value - sets value of key "key" to value
'''

class SocketListen
  def initialize port=8181
    @data = { 'apache' => 'apache', 'bsd' => 'mit', 'chef' => 'apache' }
    @port = port || 8181
    @server = TCPServer.new @port
    loop {
      client = @server.accept
      request = client.gets
      puts request
      client.puts handle request
      client.close
    }
  end

  def handle arg
    verb = arg.split()[0]
    arg=arg.split()[1..-1].join(" ")
    if verb == 'GET'
      return handle_get arg
    else
      return "NOT IMPLEMENTED"
    end
  end

  def handle_get arg
    path = (arg.split[0]).split('/')[1..-1]
    if path.nil?
      keys=[]
      @data.each do |key,value|
        keys << key
      end
      puts keys
      return keys
    else
      puts path[0] + " => " + @data[path[0]] 
      return @data[path[0]] + "    "
    end
  end


end
SocketListen.new()
