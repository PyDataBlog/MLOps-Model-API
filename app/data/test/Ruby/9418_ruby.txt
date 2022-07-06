require 'spec_helper'

describe Algorithmy do
  it 'Graph initialized' do
    graph = Algorithmy::Graph.new

    expect(graph.edge_list).to eq []
    expect(graph.node_list).to eq []
  end
end
