shared_examples 'a website with sidebar' do
  context 'sidebar' do

    it 'should exist' do
      expect(page).to have_css('#sidebar')
    end

    it 'should have only one active menu element' do
      within '#sidebar' do
        expect(page).to have_css('li.active', count: 1, text: active_element) unless active_element.nil?
      end
    end

    it 'should contain NEWS link' do
      within '#sidebar' do
        expect(page).to have_link('NEWS', href: '/', count: 1)
      end
    end

    it 'should contain PEOPLE link' do
      within '#sidebar' do
        expect(page).to have_link('PEOPLE', href: '/people/', count: 1)
      end
    end

    it 'should contain ISSUES link' do
      within '#sidebar' do
        expect(page).to have_link('ISSUES', href: '/issues/', count: 1)
      end
    end

    it 'should contain GITHUB STATISTICS link' do
      within '#sidebar' do
        expect(page).to have_link('GITHUB STATISTICS', href: '/stats/', count: 1)
      end
    end

    it 'should contain GITHUB ACTIVITIES link' do
      within '#sidebar' do
        expect(page).to have_link('GITHUB ACTIVITIES', href: '/github/', count: 1)
      end
    end

    it 'should contain link to RSS feed' do
      within '#sidebar .social' do
        expect(page).to have_link('', href: '/feed.xml')
      end
    end

    it 'should contain link to OPML feed' do
      within '#sidebar .social' do
        expect(page).to have_link('', href: '/feed.opml')
      end
    end

    it 'should contain link to TimVideos Twitter' do
      within '#sidebar .social' do
        expect(page).to have_link('', href: 'http://www.twitter.com/TimVideosUs')
      end
    end

    it 'should contain TimVideos logo' do
      within '#sidebar' do
        expect(page).to have_css('.logo', count: 1)

        within 'a.logo' do
          expect(page).to have_css('img[src="/img/logo.png"]', count: 1)
          expect(page).to have_content('PLANET', count: 1)
        end
      end
    end

    it 'should contain information about GitHub Pages' do
      within '#sidebar' do
        expect(page).to have_content('Proudly hosted by GitHub', count: 1)
        expect(page).to have_link('GitHub', href: 'http://www.github.com', count: 1)
      end
    end
  end
end
