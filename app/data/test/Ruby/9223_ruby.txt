User.find(:all).each do |user|
  user.plugins.create(:name => "refinerycms_cradle_roll",
                      :position => (user.plugins.maximum(:position) || -1) +1)
end

page = Page.create(
  :title => "Cradle Roll",
  :link_url => "/births",
  :deletable => false,
  :position => ((Page.maximum(:position, :conditions => {:parent_id => nil}) || -1)+1),
  :menu_match => "^/births.*$"
)

Page.default_parts.each do |default_page_part|
  page.parts.create(:title => default_page_part, :body => nil)
end
