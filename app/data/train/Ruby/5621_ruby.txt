require 'rails_helper'

describe Authentication do
  let(:auth) { Authentication.new(params) }
  
  let(:params) {
    {
      provider: "foo",
      uid: "12345",
      info: {
        email: "example@example.com",
        nickname: "foobar"
      }
    }
  }

  describe "when no user exists" do
    it "creates a new user" do
      expect { auth.authenticated? }.to change { User.count }.by(1)
    end
  end
  
  describe "when there is a matching user" do
    let!(:user) { FactoryGirl.create :user, provider: params[:provider], uid: params[:uid], login: params[:info][:nickname], email: params[:info][:email] }
    
    it "does not create a new user" do
      expect { auth.authenticated? }.to_not change { User.count }
    end
    
    it "returns the right user" do
      expect(auth.user).to eq(user)
    end
  end
end
