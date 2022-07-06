module ActionView
  module Helpers
    module Analytics
      module AnalyticsHelper

        def google_analytics_ua
          Rails.application.config.google_analytics_ua
        end

        def google_analytics_script_tag(ua=nil)
          ua ||= google_analytics_ua
          html = "<script type=\"text/javascript\">"
          html << "var _gaq = _gaq || [];"
          html << "_gaq.push(['_setAccount', '#{ua}']);"
          html << "(function() {"
          html << "  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;"
          html << "  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';"
          html << "  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);"
          html << "})();"
          html << "</script>"
          html.html_safe
        end

      end
    end
  end
end