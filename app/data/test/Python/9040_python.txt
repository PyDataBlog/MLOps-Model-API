# -*- coding:utf-8 -*-
from mako import runtime, filters, cache
UNDEFINED = runtime.UNDEFINED
__M_dict_builtin = dict
__M_locals_builtin = locals
_magic_number = 10
_modified_time = 1440369075.543512
_enable_loop = True
_template_filename = u'themes/monospace/templates/index.tmpl'
_template_uri = u'index.tmpl'
_source_encoding = 'utf-8'
_exports = [u'content']


def _mako_get_namespace(context, name):
    try:
        return context.namespaces[(__name__, name)]
    except KeyError:
        _mako_generate_namespaces(context)
        return context.namespaces[(__name__, name)]
def _mako_generate_namespaces(context):
    ns = runtime.TemplateNamespace(u'comments', context._clean_inheritance_tokens(), templateuri=u'comments_helper.tmpl', callables=None,  calling_uri=_template_uri)
    context.namespaces[(__name__, u'comments')] = ns

    ns = runtime.TemplateNamespace(u'helper', context._clean_inheritance_tokens(), templateuri=u'index_helper.tmpl', callables=None,  calling_uri=_template_uri)
    context.namespaces[(__name__, u'helper')] = ns

def _mako_inherit(template, context):
    _mako_generate_namespaces(context)
    return runtime._inherit_from(context, u'base.tmpl', _template_uri)
def render_body(context,**pageargs):
    __M_caller = context.caller_stack._push_frame()
    try:
        __M_locals = __M_dict_builtin(pageargs=pageargs)
        date_format = context.get('date_format', UNDEFINED)
        helper = _mako_get_namespace(context, 'helper')
        messages = context.get('messages', UNDEFINED)
        posts = context.get('posts', UNDEFINED)
        _link = context.get('_link', UNDEFINED)
        def content():
            return render_content(context._locals(__M_locals))
        comments = _mako_get_namespace(context, 'comments')
        index_teasers = context.get('index_teasers', UNDEFINED)
        __M_writer = context.writer()
        __M_writer(u'\n')
        __M_writer(u'\n')
        __M_writer(u'\n')
        if 'parent' not in context._data or not hasattr(context._data['parent'], 'content'):
            context['self'].content(**pageargs)
        

        __M_writer(u'\n')
        return ''
    finally:
        context.caller_stack._pop_frame()


def render_content(context,**pageargs):
    __M_caller = context.caller_stack._push_frame()
    try:
        date_format = context.get('date_format', UNDEFINED)
        helper = _mako_get_namespace(context, 'helper')
        messages = context.get('messages', UNDEFINED)
        posts = context.get('posts', UNDEFINED)
        _link = context.get('_link', UNDEFINED)
        def content():
            return render_content(context)
        comments = _mako_get_namespace(context, 'comments')
        index_teasers = context.get('index_teasers', UNDEFINED)
        __M_writer = context.writer()
        __M_writer(u'\n')
        for post in posts:
            __M_writer(u'        <div class="postbox">\n        <h1><a href="')
            __M_writer(unicode(post.permalink()))
            __M_writer(u'">')
            __M_writer(unicode(post.title()))
            __M_writer(u'</a></h1>\n            <div class="meta" style="background-color: rgb(234, 234, 234); ">\n                <span class="authordate">\n                    ')
            __M_writer(unicode(messages("Posted:")))
            __M_writer(u' <time class="published" datetime="')
            __M_writer(unicode(post.date.isoformat()))
            __M_writer(u'">')
            __M_writer(unicode(post.formatted_date(date_format)))
            __M_writer(u'</time>\n                </span>\n                <br>\n                <span class="tags">Tags:&nbsp;\n')
            if post.tags:
                for tag in post.tags:
                    __M_writer(u'                            <a class="tag" href="')
                    __M_writer(unicode(_link('tag', tag)))
                    __M_writer(u'"><span>')
                    __M_writer(unicode(tag))
                    __M_writer(u'</span></a>\n')
            __M_writer(u'                </span>\n            </div>\n        ')
            __M_writer(unicode(post.text(teaser_only=index_teasers)))
            __M_writer(u'\n')
            if not post.meta('nocomments'):
                __M_writer(u'            ')
                __M_writer(unicode(comments.comment_link(post.permalink(), post.base_path)))
                __M_writer(u'\n')
            __M_writer(u'        </div>\n')
        __M_writer(u'    ')
        __M_writer(unicode(helper.html_pager()))
        __M_writer(u'\n    ')
        __M_writer(unicode(comments.comment_link_script()))
        __M_writer(u'\n\t')
        __M_writer(unicode(helper.mathjax_script(posts)))
        __M_writer(u'\n')
        return ''
    finally:
        context.caller_stack._pop_frame()


"""
__M_BEGIN_METADATA
{"source_encoding": "utf-8", "line_map": {"22": 3, "25": 2, "31": 0, "45": 2, "46": 3, "47": 4, "52": 31, "58": 5, "71": 5, "72": 6, "73": 7, "74": 8, "75": 8, "76": 8, "77": 8, "78": 11, "79": 11, "80": 11, "81": 11, "82": 11, "83": 11, "84": 15, "85": 16, "86": 17, "87": 17, "88": 17, "89": 17, "90": 17, "91": 20, "92": 22, "93": 22, "94": 23, "95": 24, "96": 24, "97": 24, "98": 26, "99": 28, "100": 28, "101": 28, "102": 29, "103": 29, "104": 30, "105": 30, "111": 105}, "uri": "index.tmpl", "filename": "themes/monospace/templates/index.tmpl"}
__M_END_METADATA
"""
