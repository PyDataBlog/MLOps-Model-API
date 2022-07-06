class NullAcceptCreateColumnToDevices < ActiveRecord::Migration
  def change
        change_column :devices, :mac_address, :string, null: true, default: nil
        change_column :devices, :device_name, :string, null: false
        change_column :devices, :device_communication, :string, null: true, default: nil
  end
end
