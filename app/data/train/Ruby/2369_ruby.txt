class AddTitleEtcToProduct < ActiveRecord::Migration
  def change
	  add_column :products, :title, :string
	  add_column :products, :author, :string
	  add_column :products, :description, :string
  end
end
