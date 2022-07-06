class RemoveVersionFromUcpengineEntries < ActiveRecord::Migration
  def change
    remove_column :ucpengine_entries, :version, :decimal
  end
end
