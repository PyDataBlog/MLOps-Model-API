class CreateInventoryItems < ActiveRecord::Migration
  def change
    create_table :inventory_items do |t|
      t.boolean :sold
      t.integer :price

      t.timestamps
    end
  end
end
