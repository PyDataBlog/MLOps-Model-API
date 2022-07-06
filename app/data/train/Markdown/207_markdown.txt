---
layout: page
permalink: /sobre/index.html
title: Interface
tags: [Interface, Pesquisa, Filosofia, Cultura]
imagefeature: fourseasons.jpg
chart: true
---
<figure>
  <img src="{{ site.url }}/images/profile.png" alt="The Immortal">
  <figcaption><a href="http://www.nickgentry.com/the-immortal/" target="_blank">The Immortal, de Nick Gentry</a></figcaption>
</figure>

{% assign total_words = 0 %}
{% assign total_readtime = 0 %}
{% assign featuredcount = 0 %}
{% assign statuscount = 0 %}

{% for post in site.posts %}
    {% assign post_words = post.content | strip_html | number_of_words %}
    {% assign readtime = post_words | append: '.0' | divided_by:200 %}
    {% assign total_words = total_words | plus: post_words %}
    {% assign total_readtime = total_readtime | plus: readtime %}
    {% if post.featured %}
    {% assign featuredcount = featuredcount | plus: 1 %}
    {% endif %}
{% endfor %}


O Grupo Interdisciplinar de Pesquisa em Filosofia e Cultura funciona junto ao [Instituto Federal de Brasília](http://www.ifb.edu.br/pesquisa/pesquisa3/grupos-de-pesquisa-2), Campus Brasília, em articulação com a Licenciatura em Dança e com os cursos Técnicos de Informática, Eventos e Serviços Públicos. Além disso, o grupo visa a realização de pesquisas e atividades conjuntas com pesquisadores de outros Campus do IFB.
     
O Grupo surge por iniciativa de pesquisadores interessados em criar um espaço comum para o incentivo à discussão e ao desenvolvimento e divulgação de pesquisas nas áreas de Filosofia e Cultura com abordagens interdisciplinares. A atuação do grupo se dará, principalmente, sob a forma de participação e realização de encontros, cursos, eventos científicos e artísticos, colóquios e publicações.
     
As principais áreas de interesse do grupo são:
     
1. Filosofia da Arte
2. Corpo
3. Tecnologia digital
     
Acesse também a [página do grupo no diretório de grupos do CNPq](http://dgp.cnpq.br/dgp/espelhogrupo/6419069911099653).
     
***
     
A imagem utilizada como avatar do site ([The Immortal](http://www.nickgentry.com/the-immortal/)) é de autoria do artista londrino [Nick Gentry](http://www.nickgentry.com). O artista utiliza em suas composições materiais obsoletos, como disquetes, HDs antigos etc., representando a efemeridade de nossos aparatos tecnológicos e promovendo uma reflexão sobre temas como identidade, consumo e relações sociais.
