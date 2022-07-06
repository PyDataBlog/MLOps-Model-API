import { StethoscopePage } from './app.po';

describe('stethoscope App', function() {
  let page: StethoscopePage;

  beforeEach(() => {
    page = new StethoscopePage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
