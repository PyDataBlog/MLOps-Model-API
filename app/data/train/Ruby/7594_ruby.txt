require "spec_helper"

module Craps 
 
  describe Dice do

    describe "#base_prob" do
      let(:dice) { Craps::Dice.new(6) }

      it "returns floating number" do  
        expect(dice.base_prob).to be_an(Float)
      end

      it "return correct result" do  
        expect(dice.base_prob).to eq(1.0/6.0)
      end
    
    end

    describe "#higher_or_equal_to" do
      let(:dice) { Craps::Dice.new(6) }

      it "returns floating number" do  
        expect(dice.higher_or_equal_to(3)).to be_an(Float)
      end

      it "return correct result" do  
        expect(dice.higher_or_equal_to(5)).to eq(1.0 - (5.0/6.0)+(1.0/6.0))
      end
    
    end

    describe "#lower_or_equal_to" do
      let(:dice) { Craps::Dice.new(6) }

      it "returns floating number" do  
        expect(dice.lower_or_equal_to(3)).to be_an(Float)
      end

      it "return correct result" do  
        expect(dice.lower_or_equal_to(5)).to eq(5.0/6.0)
      end
    
    end

    #describe "#valid_integer" do
    #  let(:dice) { Craps::Dice.new(6) }
#
    #  it "returns floating number" do  
    #    expect(dice.base_prob).to be_an(Float)
    #  end
#
    #  it "returns 16.66666%" do  
    #    expect(dice.base_prob).to eq(1.0/6.0)
    #  end
    #
    #end


  end

end