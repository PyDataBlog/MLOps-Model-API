# imports
## core

import importlib
import logging
import os
import pprint
import sys
import StringIO

## 3rd party
import cherrypy
import requests


## local
def full_path(*extra):
    return os.path.join(os.path.dirname(__file__), *extra)
sys.path.insert(0, full_path())

import db

logging.basicConfig()

sorry = 'This is only for US Citizens. Sorry and thank you for your time.'

class Root(object):

    @cherrypy.expose
    def index(self, tag):
        redirect_url = db.urls[tag]
        ip = cherrypy.request.headers['Remote-Addr']
        request_url = 'http://ipinfo.io/{0}/country'.format(ip)
        r = requests.get(request_url)
        country = r.text.strip()
        if country == 'US':
            raise cherrypy.HTTPRedirect(redirect_url)
        else:
            return sorry
