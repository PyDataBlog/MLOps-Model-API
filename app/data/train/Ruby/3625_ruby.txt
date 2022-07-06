class CreateYyCategories < ActiveRecord::Migration
  def change
    create_table :yy_categories do |t|
      t.string :name, null: false
      t.text :description

      t.timestamps
    end
    add_index :yy_categories, :name, unique: true
  end
end
