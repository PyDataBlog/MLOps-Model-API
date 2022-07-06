require 'rails_helper'

feature 'user login' do
  scenario 'Login' do
    visit '/'
    expect(page).to have_button 'Sign in'
  end

  scenario 'Login' do
    visit '/'
    expect(find('#user_email')).to_not be_nil
  end

  scenario 'Login' do
    visit '/'
    expect(find('#user_password')).to_not be_nil
  end
end


feature 'user sign in' do
  scenario 'Signin' do
    visit '/'
    expect(page).to have_link 'Sign up'
  end
end