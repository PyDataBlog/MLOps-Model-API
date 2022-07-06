# -*- coding: utf-8 -*-
from scrapy.spider import Spider
from scrapy.selector import Selector
from kgrants.items import KgrantsItem
from scrapy.http import Request
import time



class GrantsSpider(Spider):
    name = "grants"
    allowed_domains = ["www.knightfoundation.org"]
    pages = 1
    base_url = 'http://www.knightfoundation.org'
    start_url_str = 'http://www.knightfoundation.org/grants/?sort=title&page=%s'



    def __init__(self, pages=None, *args, **kwargs):
        super(GrantsSpider, self).__init__(*args, **kwargs)

        if pages is not None:
            self.pages = pages
            self.start_urls = [ self.start_url_str % str(page) for page in xrange(1,int(self.pages)+1)]





    def parse(self, response):

        hxs = Selector(response)
        projects = hxs.xpath('//article')

        for project in projects:
            time.sleep(2)
            project_url = self.base_url + ''.join(project.xpath('a/@href').extract())
            grants = KgrantsItem()
            grants['page'] = project_url
            grants['project'] = ''.join(project.xpath('a/div/header/h1/text()').extract()).strip()
            grants['description'] = ''.join(project.xpath('p/text()').extract()).strip()
            yield Request(grants['page'],
                          callback = self.parse_project,
                          meta={'grants':grants})


    def parse_project(self,response):

        hxs = Selector(response)
        grants = response.meta['grants']
        details = hxs.xpath('//section[@id="grant_info"]')
        fields = hxs.xpath('//dt')
        values = hxs.xpath('//dd')
        self.log('field: <%s>' % fields.extract())

        for item in details:
            grants['fiscal_agent'] = ''.join(item.xpath('header/h2/text()').extract()).strip()
            count = 0
            for field in fields:
                normalized_field = ''.join(field.xpath('text()').extract()).strip().lower().replace(' ','_')
                self.log('field: <%s>' % normalized_field)
                try:
                    grants[normalized_field] = values.xpath('text()').extract()[count]
                except:
                    if normalized_field == 'community':
                        grants[normalized_field] = values.xpath('a/text()').extract()[1]
                    elif normalized_field == 'focus_area':
                        grants[normalized_field] = values.xpath('a/text()').extract()[0]

                count += 1
            grants['grantee_contact_email'] = ''.join(
                item.xpath('section[@id="grant_contact"]/ul/li[@class="email"]/a/@href').extract()).replace('mailto:','').strip()
            grants['grantee_contact_name'] = ''.join(
                item.xpath('section[@id="grant_contact"]/ul/li[@class="email"]/a/text()').extract()).strip()
            grants['grantee_contact_location'] = ''.join(
                item.xpath('section[@id="grant_contact"]/ul/li[@class="location"]/text()').extract()).strip()
            grants['grantee_contact_facebook'] = ''.join(
                item.xpath('section[@id="grant_contact"]/ul/li[@class="facebook"]/a/@href').extract()).strip()
            grants['grantee_contact_twitter'] = item.xpath('section[@id="grant_contact"]/ul/li[@class="twitter"]/a/@href').extract()
            grants['grantee_contact_website'] = item.xpath('section[@id="grant_contact"]/ul/li[@class="website"]/a/@href').extract()
            if 'grant_period' in grants:
                grant_period = grants['grant_period'].split(' to ')
                grants['grant_period_start'] = grant_period[0]
                grants['grant_period_end'] = grant_period[1]

            yield grants

