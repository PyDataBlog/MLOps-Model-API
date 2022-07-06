from django.conf.urls import url
from blog import views


urlpatterns = [
    url(r'^view$', views.archive),
    url(r'^$', views.welcome),
    url(r'^create', views.create_blogpost),
]
