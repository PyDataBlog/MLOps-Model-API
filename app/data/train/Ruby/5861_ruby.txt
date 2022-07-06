# encoding: utf-8
#

require 'spec_helper'
require '000/sha1_bitwise_compress0'
require '001/sha1_bitwise_compress0_f088'

describe SHA1BitwiseCompress0_F088, '#compress_0' do
  it 'detects when bit 088 is true' do
    expect { SHA1BitwiseCompress0_F088.new.compress_0([0x00000000, 0x00000000, 0x00000100, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000]) }.to raise_error(ArgumentError)
  end

  it 'detects when input vector is correct' do
    expect { SHA1BitwiseCompress0_F088.new.compress_0([0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000]) }.not_to raise_error
  end

  it 'calculates 512 * false + restrictions correctly' do
    iv = [0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000]
    expected = SHA1BitwiseCompress0.new.compress_0(iv)
    actual = SHA1BitwiseCompress0_F088.new.compress_0(iv)
    expect(actual).to eq(expected)
  end
end
