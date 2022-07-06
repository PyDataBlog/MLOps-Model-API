class CreateBanksTable < ActiveRecord::Migration
  def change
    create_table :banks do |t|
      t.references :agendas
      t.references :users
      t.timestamps
    end
  end
end
