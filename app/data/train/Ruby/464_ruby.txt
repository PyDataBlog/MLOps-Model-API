class CreateIp4NetworkLocationJoinTable < ActiveRecord::Migration
  def change
    create_join_table :ip4_networks, :locations do |t|
      t.index [:ip4_network_id,:location_id], unique: true, name: 'ip4_network_location'
    end
  end
end
