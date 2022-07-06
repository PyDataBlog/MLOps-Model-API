---
layout: post
title: How to set up Google Hangouts on Air for Workshop Streaming
description: "Tips for remote teaching"
modified: 2016-03-29
tags: [teaching]
image:
  feature: googlehangout-pic.png
  credit:
  creditlink:
comments: true
share: true
---
So you want to broadcast and archive a workshop (or something else) to the Internet?

I organize a lot of [training workshops](http://dib-training.readthedocs.org/en/pub/) for my job as a 
Training Coordinator in Titus Brown's lab. Recently we've been broadcasting some of our workshops using
Google Hangouts on Air, which streams our lesson to YouTube and saves the video on the same page. 

This post is a guide on how to set up the Hangouts, along with some tips I've learned. I'm happy to
take questions on this process as well!

### Done Before the Workshop

*Setting Up the Hangouts:*

1. Go to the [Hangouts on Air page](https://plus.google.com/hangouts/onair), click yellow "Create Hangout on Air" button. You also must make sure you have a verified YouTube account associated with the gmail account of the person hosting the hangout.

2. Give it a name, description (optional), click starts later and specify time to start. Helpfully,
the Hangout won't automatically start at that time, you still need to press the "Go Live" button in 
the Hangout to start the streaming to YouTube, which you can do before or after the specified time.
Also, I always overestimate the duration of the Hangout, because you can end it at any time. So for
a three hour workshop I'll set it to five hours. If you set this too short, the streaming to YouTube
may stop early and that portion of the video will not be saved.

3. Under "Audience", delete "Public". This would allow anyone to join your Hangout on Air call. 
Because we are recording lectures, we don't want people we don't know joining in and possibly
interrupting the lesson. I put the email address of another instructor here because it will
complain if that box is left blank.

4. Click "Share". This will take you to the Google+ page for the Hangout. 

*To get the links:* Click "Start", and "Skip" on the Invite Guests pop up screen (though it doesn't
hurt anything to invite people because you are not streaming at this point). In the Hangout screen,
click "Links", and this will give you relevant links for the Hangout. Our lab only uses the YouTube 
link here because our Hangout is private, so people can't join from the Google+ Hangout page anyway.

*To invite people:* In the Hangout screen, click the "Invite People" icon at the top center of the
screen. This will give you a pop up screen with the permanent sharing link. I've found that it is easiest
to send this link to whomever you want to join the Hangout instead of messing with giving people access 
via email accounts.

#### Creating and Distributing Materials

Before the workshop, we will also make an Etherpad which allows remote sites or students in our own 
classroom to ask questions (and of course serves as a collaborate note taking tool/place to post
code snippets). Once I have the YouTube link, I can go to that page and put the Etherpad and link to
our lesson materials in the video description. I used to tweet out the link to the Etherpad with the
YouTube link, but we would get spammers (I guess this is a thing?) posting text ads all over the 
Etherpad. It is kind of fun to locate and delete spam as it happens, but maybe not the most conducive
to a learning environment.

#### Remote Sites

Remote classrooms have someone in charge of making sure questions get asked (or the instructor themselves)
as well as several helpers.

We'll do a tech test with remote sites to make sure that their video and audio are correctly working.
You can join the actual Hangout for the workshop (nothing is recorded until you hit "go live") or make 
a test one.

#### During the Workshop

To make the Hangout live and stream it to YouTube, click the green "Start Broadcast" button at the 
bottom of the Hangout screen. Before you start the broadcast, you can join and leave and invite others
who join and leave the hangout as many times as you want before starting the broadcast. An important point
is that you do **not** want to click "Stop Broadcast" before you are done. Once this is clicked, you can't
go back to streaming (to my knowledge). If this is accidentally done before the lesson is done, a new Hangout
needs to be created with a new YouTube link, which is a hassle and problematic for people watching the stream.

During the stream, the creator of the Hangout can click on the lecturer's screen to designate that it is 
always displayed to everyone. If this isn't done, the screen will switch whenever another site talks into
their mic. All remote sites should have their mics muted until they want to ask a question to the lecturer.
Also, as the Hangout creator, you can adjust sound levels or mute mics in the "Control Room" within the 
Hangout window, though I rarely have a reason to do this. The lecturer must click "Share Screen" in their
Hangout window.

We don't have live chat on the YouTube video itself because the majority of people in classrooms don't have
the YouTube video on their computers, and the Etherpad has worked very well for taking questions from people. 

During the stream itself, people can leave and come back to the stream without an issue. We've had the lecturer
disconnect from the stream during a coffee break and reconnect back with no problem. The YouTube video will 
display any other videos on the screen. Personally I try to put a message on the screen saying we will be back
either by sharing my own screen displaying text which says that, or the low-tech way of writing it on papers and
displaying it to the camera.

#### After the Workshop

The YouTube video remains in the same spot as the stream link. I change the video from "Unlisted" (the default
if you do a private Hangout, meaning only those with the link can view it) to "Public". 

I used to spend a lot of time going back and putting time stamps in video descriptions for what happens when (like
[this](https://www.youtube.com/watch?v=eIrZjVH0Zcg) for example), but I don't know how helpful this is to people
if the lecture materials are already linked in the description. 

#### Failure Modes

* We unfortunately had our awesome [d3.js workshop](https://www.youtube.com/watch?v=eIrZjVH0Zcg) stream taught by Emily Dolson as MSU get cut off early, though the Hangout continued to work fine. This was our first live streamed workshop, and my guess about why that happened is that the length of the Hangout was automatically specified to be a shorter amount of time than the workshop. Since then, I've always specified that the length of stream be much longer than necessary while setting it up and this problem hasn't occurred again. I also make sure to monitor the YouTube watch page to ensure it's still running correctly, though if anything went wrong, all I could do is make a new Hangout and YouTube stream.
* We have had poor internet connections result in less than stellar audio and video, or even cause an instructor to be dropped out of a stream (but they can rejoin when their connection returns!). A poor connection can also result in a blurry stream video, but it seems like the saved videos are sharper. There's not a great solution to this besides being connected via ethernet.
* We once had a remote site need to leave our lesson early because they were having a fire drill. Luckily, it was just a drill and they were able to finish the lesson by watching the video! Not much to prevent this problem expect maybe eliminating fires and the need for drills, which is unlikely.


#### Workshops we've one so far using this technique:

* [Short-read trimming and quality evaluation - half day workshop (Titus Brown)](https://www.youtube.com/watch?v=_nNq4kq1Wx0)
* [Amazon Web Services (Titus Brown)](https://www.youtube.com/watch?v=IFdBD3YdLJc)
* [Sphinx and Bitbucket (Adelaide Rhodes)](https://www.youtube.com/watch?v=ughHAjjM7Fc)
* [scipy.optimize (Ariel Rokem)](http://www.youtube.com/watch?v=0eFokR-ikaA)
* [Regular Expressions & Python (Tiffany Timbers)](https://www.youtube.com/watch?v=GklxBhgUR4g)
* [Advanced git (Raniere Silva)](https://www.youtube.com/watch?v=JTnIDMn47Pk&feature=youtu.be)
* [pydoit for Workflow Automation (Camille Scott)](http://www.youtube.com/watch?v=EfD9bWmL-1M&t=20m20s)
* [d3.js for Interactive Data Visualization (Emily Dolson)](https://www.youtube.com/watch?v=eIrZjVH0Zcg)

I won't update this list here, but additional videos and future streams (including an Advanced Beginner 
Python from Titus and a Beginner and then Intermediate Git lesson from Daniel Chen) are posted on the
[DIB Training Workshop Page](http://dib-training.readthedocs.org/en/pub/). As always, materials from past
workshops are there as well for reference.

I hope this information is help to those wanting to stream lessons. I think this is a great tool for teaching, and I have emails from thankful people in my inbox to prove it!
