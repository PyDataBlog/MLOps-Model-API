require 'spec_helper'
require 'feature/testing'

describe 'Feature testing support' do

  let(:repository) { Feature::Repository::SimpleRepository.new }

  before(:each) do
    repository.add_active_feature(:active_feature)
    Feature.set_repository(repository)
  end

  it 'should execute code block with an deactivated feature' do
    expect(Feature.active?(:another_feature)).to be_falsey

    Feature.run_with_activated(:another_feature) do
      expect(Feature.active?(:another_feature)).to be_truthy
    end

    expect(Feature.active?(:another_feature)).to be_falsey
  end

  it 'should execute code block with an deactivated feature' do
    expect(Feature.active?(:active_feature)).to be_truthy

    Feature.run_with_deactivated(:active_feature) do
      expect(Feature.active?(:active_feature)).to be_falsey
    end

    expect(Feature.active?(:active_feature)).to be_truthy
  end

  context 'Multiple features' do

    before(:each) do
      repository.add_active_feature(:second_active_feature)
      Feature.set_repository(repository)
    end

    it "should enable multiple deactivated features" do
      expect(Feature.active?(:active_feature)).to be_truthy
      expect(Feature.active?(:second_active_feature)).to be_truthy
    end

    it "should execute code block with multiple deactivated features" do
      Feature.run_with_activated([:active_feature, :second_active_feature]) do
        expect(Feature.active?(:active_feature)).to be_truthy
        expect(Feature.active?(:second_active_feature)).to be_truthy
      end
    end

  end
end
