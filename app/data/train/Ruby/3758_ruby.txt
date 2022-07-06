require_relative "spec_helper"

describe ExtraDoilies::CounterColumn do
  let(:counter) { ExtraDoilies::CounterColumn.new(name, options) }

  it "counts up" do
    expect(counter.value(object,view)).to eq(1)
    expect(counter.value(object,view)).to eq(2)
  end
end
