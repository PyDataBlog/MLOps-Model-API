class AddStatusToHomeWorks < ActiveRecord::Migration
  def change
    add_column :home_works, :status, :string
  end
end
