class CreateReactors < ActiveRecord::Migration
  def change
    create_table :reactors do |t|
      t.integer :death_star_id
      t.integer :metal_id
      t.string :name
      t.decimal :power_output
      t.decimal :x
      t.decimal :y
      t.decimal :z
      # t.integer :material_id
      # t.string :material_type

      t.timestamps
    end
  end
end
