class TempGenerator
  attr_reader :date, :filename, :title, :file_array

  def initialize(date)
    @date = date
  end

  def to_file
    if File.exist? @filename
      puts "The file already exists. Quitting"
      return
    end
    File.open(@filename, 'w') do |f|
      @file_array.each { |line| f.write line }
    end
    puts "File written to #{@filename}"
  end
end
