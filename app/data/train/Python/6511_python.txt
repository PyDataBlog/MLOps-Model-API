python manage.py collectstatic

python manage.py runserver --nostatic

urlpatterns += patterns('',
    (r'^static/suit/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.DJANGO_SUIT_TEMPLATE}),
)
urlpatterns += patterns('',
    (r'^static/admin/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.DJANGO_ADMIN_TEMPLATE}),
)

SITE_PATH = os.path.dirname(__file__)
REPO_ROOT = os.path.normpath(os.path.join(SITE_PATH, '..'))
MEDIA_ROOT = os.path.join(REPO_ROOT, 'public/media')
DJANGO_SUIT_TEMPLATE = os.path.join(REPO_ROOT, 'static/suit')
DJANGO_EDITOR = os.path.join(REPO_ROOT, 'static/django_summernote')
DJANGO_ADMIN_TEMPLATE = os.path.join(REPO_ROOT, 'static/admin')
