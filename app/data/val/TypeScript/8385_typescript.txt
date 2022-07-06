import {LoginPage} from "./LoginPage";
import { browser, element, by } from 'protractor';

describe('url-minifie App', function() {
  let page: LoginPage;

  beforeEach(() => {
    page = new LoginPage();
  });

  it('must show an empty email and empty password error', () => {
    page.navigateTo();
    page.email.sendKeys('');
    page.password.sendKeys('');
    page.submitButton.click();
    expect(page.errorMessage.getText()).toEqual('Le courriel et le mot de passe sont vide');
  });

  it('must show an empty password error', () => {
    page.navigateTo();
    page.email.sendKeys('test@protractor.com');
    page.password.sendKeys('');
    page.submitButton.click();
    expect(page.errorMessage.getText()).toEqual('Le mot de passe est vide');
  });

  it('must show an empty email error', () => {
    page.navigateTo();
    page.email.sendKeys('');
    page.password.sendKeys('password');
    page.submitButton.click();
    expect(page.errorMessage.getText()).toEqual('Le courriel est vide');
  });

  it('must redirect login to dashboard', () => {
    page.navigateTo();
    page.email.sendKeys('test@protractor.com');
    page.password.sendKeys('password');
    page.submitButton.click();

    browser.manage().timeouts().pageLoadTimeout(10000);
    expect(browser.getCurrentUrl()).toBe('http://localhost:4200/dashboard');
  });
});
