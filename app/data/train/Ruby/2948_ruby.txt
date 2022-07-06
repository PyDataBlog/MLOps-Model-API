require 'not_magic'

class Item
  def available?
    true
  end
end

describe 'NotMagic','adds not_ appended methods to all objects' do
  context 'on a class that has an available? method that returns true' do
    it 'adds a not_available? method' do
      item = Item.new
      item.not_available?.should eq false
    end
  end
end
