require 'spec_helper'

describe Managr::Taggable do
  
  let!(:task) do
    Managr::Task.make
  end
  
  describe 'associations' do
    
    it 'has many taggings' do
      task.should have_many(
        :taggings)
    end
    
    it 'has many tags through taggings' do
      task.should have_many(
        :tags).through(:taggings)
    end
  end

end