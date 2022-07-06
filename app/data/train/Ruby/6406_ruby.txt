class MessageListAction < Cramp::Action
  def start
    para = params
    ap para
    render "This is MessageList!"
    finish
  end
end
