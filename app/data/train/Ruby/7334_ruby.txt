class Trolldrop < Formula
  homepage "https://github.com/neonichu/trolldrop"
  url "https://github.com/neonichu/trolldrop.git", :tag => "0.0.1"
  head "https://github.com/neonichu/trolldrop.git"

  depends_on :xcode => ["7.2"]

  def install
    system 'xcodebuild'
    system 'mkdir', '-p', "#{prefix}/bin"
    system 'cp', 'build/Release/trolldrop', "#{prefix}/bin/trolldrop"
  end
end
