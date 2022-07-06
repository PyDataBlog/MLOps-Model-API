module DocFrac
  class Format
    attr_reader :format_text, :ext
    def initialize(format)
      formats = {
        :to_rtf => "--to-rtf",
        :from_rtf => "--from-rtf",
        :to_html => "--to-html",
        :from_html => "--from-html",
        :to_text => "--to-text",
        :from_text => "--from-text"
      }
      @format_text = formats[format]
      @ext = format.to_s.split("_").last
    end
  end
end