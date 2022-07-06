# -*- coding: utf-8
from __future__ import unicode_literals, absolute_import

from django.conf.urls import url, include

from unach_photo_server.urls import urlpatterns as unach_photo_server_urls

urlpatterns = [
    url(r'^', include(unach_photo_server_urls, namespace='unach_photo_server')),
]
