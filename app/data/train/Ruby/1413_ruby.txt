class CreateArticles < ActiveRecord::Migration
  def self.up
    Cms::ContentType.create!(:name => "BcmsMy401kLibrary::Article", :group_name => "My401k Product")
  end

  def self.down
    raise "down migrate this manually"
  end
end
