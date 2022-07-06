class CreateSparkPlugsStaticPages < ActiveRecord::Migration
  def change
    create_table :spark_plugs_static_pages do |t|
      t.string :page, null: false, unique: true
      t.string :title, null: false
      t.text :content

      t.timestamps null: false
    end
  end
end
