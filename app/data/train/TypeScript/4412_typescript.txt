import { FriendlyEnginePage } from './app.po';

describe('friendly-engine App', () => {
  let page: FriendlyEnginePage;

  beforeEach(() => {
    page = new FriendlyEnginePage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
