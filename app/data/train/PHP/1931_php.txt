<div>
{% for new in news %}
    <div widget-toggle="news" id_widget_note="{{new.id_article}}">
        <h2 change_key="title" replace="input">{{new.title}}</h2>
        <div change_key="content" replace="ck">{{new.content}}</div>
    </div>
{% endfor %}
<br clear="both">
</div>