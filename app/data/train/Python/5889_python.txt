from __future__ import unicode_literals
from future.builtins import int

from collections import defaultdict

from django.core.urlresolvers import reverse
from django.template.defaultfilters import linebreaksbr, urlize

from mezzanine import template
from mezzanine.conf import settings
from mezzanine.generic.forms import ThreadedCommentForm
from mezzanine.generic.models import ThreadedComment
from mezzanine.utils.importing import import_dotted_path
from mezzanine.pages.models import Page, RichTextPage

register = template.Library()

@register.assignment_tag
def allpages():
    page_fields = [ 'content', 'created', 'description', 'expiry_date', 'gen_description', u'id',  'keywords', u'keywords_string',  'publish_date', 'short_url', 'slug', 'status', 'title', 'titles', 'updated']
    output = []
    # import pdb;pdb.set_trace()
    AllPages = RichTextPage.objects.all()
    for item in AllPages:
        temp = {}
        for fld in page_fields:
            temp[fld] = getattr(item, fld)
        output.append(temp)
    return {
        'pages': output
    }

@register.filter()
def remove_slash(value):
    return '#' + value[1:-1]


@register.filter()
def lower(value):
    # import pdb;pdb.set_trace()
    return value.lower()