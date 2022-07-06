class CreateProjects < ActiveRecord::Migration
  def change
    create_table :projects do |t|
      t.string :title
      t.string :sales_pitch
      t.string :description
      t.belongs_to :category, index: true

      t.timestamps null: false
    end
  end
end
