require 'spec_helper'

feature 'Login' do
  describe 'from main page' do
    before(:each) do
      visit '/'
    end
    it 'form design and content is OK' do
      within('form.form-inline') do
        expect(page).to have_css('input[placeholder=Email][type=email]')
        expect(page).to have_css('input[placeholder=Password][type=password]')
      end

      expect(page).not_to have_link('Logout')
    end

    it 'should be remeber me checkbox' do
      expect(page).to have_content('Remember me')
      check('Remember me')
    end

    it 'should be forgot password link' do
      within('form') do
        expect(page).to have_link('Forgot your password?')
      end
    end

    describe 'new user' do
      before(:each) do
        @user = FactoryGirl.create(:user, password: 'qwe12345', password_confirmation: 'qwe12345')
      end

      it 'should be able to login' do
        fill_in 'Email', with: @user.email
        fill_in 'Password', with: 'qwe12345'
        click_button 'Sign in'

        expect(page).not_to have_button 'Sign in'
        expect(page).to have_link 'Logout'
      end
    end
  end
end
