require 'spec_helper'

describe ActiveNothing::Thing do
  describe '#build' do
    let(:expected) { 'wat' }
    let(:expected1) { 'taw' }

    it 'builds object with key as method and return its value' do
      thing = described_class.build(name: expected)
      expect(thing.name).to be(expected)
    end

    it 'builds object with keys as methods and returns their values' do
      thing = described_class.build(first_name: expected, last_name: expected1)
      expect(thing.first_name).to be(expected)
      expect(thing.last_name).to be(expected1)
    end

    it 'raises ArgumentError if passed empty hash' do
      expect { described_class.build({}) }.to raise_error(ArgumentError)
    end
  end
end
