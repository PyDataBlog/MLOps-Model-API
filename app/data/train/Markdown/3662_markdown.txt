---
layout: post
title: "Agile Iowa Social - November"
categories: events social
event:
  facebook-id: 
  eventbrite-id: agile-iowa-social-november-2018-tickets-51480275891
  date: 2018-11-08
  time: 5:00 PM to 8:00 PM

  presenter:
    name: 
    title: 
    bio:

  location:
    name: The Walnut
    address: 1417 Walnut St
    citystatezip: Des Moines, IA 50309
    instructions: 

  description: 
  slides: 

---
**{{page.event.date | date: "%A, %B %-d, %Y" }} from
 {{page.event.time}}**
{% if page.event.slides %}
  **Download the slides:**
  [{{page.event.slides}}](p/{{page.event.slides}})
{% endif %}

**Location**  
{{page.event.location.name}}  
{{page.event.location.address}}  
{{page.event.location.citystatezip}}  

{% if page.event.location.instructions %}
  *Additional instructions*: 
  {{page.event.location.instructions}}
{% endif %}

<a class="btn" title="EventBrite Registration"  href="http://www.eventbrite.com/e/{{page.event.eventbrite-id}}" target="_blank" data-eventdate="{{page.event.date | date: '%D'}}">Register on EventBrite</a>

**Description**  
 The format of this meeting is simple:

  - Show up
  - Order Drinks and/or Food (optional)
  - Socialize

I hope to see you there!

{{page.event.description | markdownify}}

<a class="btn" title="EventBrite Registration" href="http://www.eventbrite.com/e/{{page.event.eventbrite-id}}" target="_blank">Register on EventBrite</a>
