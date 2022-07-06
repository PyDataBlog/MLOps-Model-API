class AddBodyUseridToTweet < ActiveRecord::Migration
  def up
    add_column :tweets, :body, :text
    add_column :tweets, :user_id, :integer
  end

  def down
    remove_column :tweets, :body, :text
    remove_column :tweets, :user_id, :integer
  end
end
