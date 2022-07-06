=begin

Create a spec/cell_spec.rb file that requires
the spec_helper and is setup so tests can be added.
=end

# spec/cell_spec.rb
require "spec_helper"

module TicTacToe
  describe Cell do

    context "#initialize" do
      it "is initialized with a value of '' by default" do
        cell = Cell.new
        expect(cell.value).to eq ''
      end

      it "can be initialized with a value of 'X'" do
        cell = Cell.new("X")
        expect(cell.value).to eq "X"
      end
    end

  end
end