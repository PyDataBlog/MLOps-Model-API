from django.conf.urls import url,include
from django.contrib import admin
from cn_device import views
urlpatterns = [
    url(r'^admin/', admin.site.urls),
    url(r'^send/(?P<id_ras>[0-9]+)/$',views.payam,name='send condition'),
    url(r'^give/(?P<id_ras>[0-9]+)/(?P<bl>[0-1])/$', views.give_req, name='give condition'),

]
