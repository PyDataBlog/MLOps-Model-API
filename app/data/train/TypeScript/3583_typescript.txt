import { RecipiePlayerPage } from './app.po';

describe('recipie-player App', function() {
  let page: RecipiePlayerPage;

  beforeEach(() => {
    page = new RecipiePlayerPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
