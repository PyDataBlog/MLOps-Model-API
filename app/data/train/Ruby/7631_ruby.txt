require 'rails_helper'

RSpec.describe Element, type: :model do
 
  describe 'when name is not present' do
    it 'should be invalid' do

      expect{ create(:element_invalid)}.to raise_error(ActiveRecord::RecordInvalid)
      
    end
  end
end
