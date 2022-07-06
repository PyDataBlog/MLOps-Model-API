# Jrails-ui
module ActionView
	module Helpers
		module JavaScriptHelper
		  
      def hash_to_jquery(hash)
        '{'+hash.map { |k,v|
          [k,
            case v.class.name
            when 'String'
              "'#{v}'"
            when 'Hash'
              hash_to_jquery(v)
            else
              v.to_s
            end
          ].join(':')
        }.join(',')+'}'
      end
  
      def flash_messages
        content_tag(:div, :class => "ui-widget flash") do
          flash.map { |type,message|
            state = case type
              when :notice; 'highlight'
              when :error; 'error'
            end
            icon = case type
              when :notice; 'info'
              when :error; 'alert'
            end
            content_tag(:div, content_tag(:span, "", :class => "ui-icon ui-icon-#{icon}", :style => "float: left; margin-right: 0.3em;")+message, :class => "ui-state-#{state} ui-corner-all #{type}", :style => "margin-top: 5px; padding: 0.7em;")
          }.join
        end
      end

      def link_to_dialog(*args, &block)
        div_id = "dialog#{args[1].gsub(/\//,'_')}"

        dialog = {
          :modal => true,
          :draggable => false,
          :resizable => false,
          :width => 600
        }.merge(args[2][:dialog] || {})
    
        args[2].merge!(
          :onclick => "$('##{div_id}').dialog(#{hash_to_jquery(dialog)});return false;"
        )
    
        args[2].delete(:dialog)
    
        if block_given?
          dialog_html = capture(&block)
        else
          render_options = args[2][:render] || {}
          dialog_html = render(render_options)
        end
    
        link_html = link_to(*args)+content_tag(:div, dialog_html, :style => 'display:none;', :id => div_id, :title => args.first)
    
        return block_given? ? concat(link_html) : link_html
      end
  
    end
  end
end 
