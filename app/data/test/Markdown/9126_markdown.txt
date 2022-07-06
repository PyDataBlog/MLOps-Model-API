---
layout: post
status: publish
published: true
title: Drizzle 2011.02.10 (RC) tarball has been released
author: Patrick Crews
author_login: pcrews
author_email: gleebix@gmail.com
wordpress_id: 1031
wordpress_url: http://blog.drizzle.org/?p=1031
date: 2011-02-15 10:10:00.000000000 +00:00
categories:
- Release
tags: []
comments:
- id: 72
  author: Tweets that mention drizzle.org -- Topsy.com
  author_email: ''
  author_url: http://topsy.com/blog.drizzle.org/2011/02/15/drizzle-2011-02-10-tarball-has-been-released/?utm_source=pingback&amp;utm_campaign=L2
  date: '2011-02-15 11:34:52 +0000'
  date_gmt: '2011-02-15 19:34:52 +0000'
  content: '[...] This post was mentioned on Twitter by drizzlenews, drizzlenews.
    drizzlenews said: DrizzleBlog Drizzle 2011.02.10 tarball has been released: Drizzle
    source tarball, version 2011.02.10 has been re... http://bit.ly/gr4pEX [...] '
- id: 73
  author: Tweets that mention drizzle.org -- Topsy.com
  author_email: ''
  author_url: http://topsy.com/trackback?url=http%3A%2F%2Fblog.drizzle.org%2F2011%2F02%2F15%2Fdrizzle-2011-02-10-tarball-has-been-released%2F%3Fat_xt%3D4d5b3511d5c66272%252C0%26sms_ss%3Dtwitter&amp;utm_source=pingb
  date: '2011-02-16 01:04:25 +0000'
  date_gmt: '2011-02-16 09:04:25 +0000'
  content: '[...] This post was mentioned on Twitter by MySQL Boy, Drizzle DB Project.
    Drizzle DB Project said: Drizzle 2011.02.10 (RC) has been released http://t.co/9acIWfs
    [...] '
- id: 74
  author: workcenter220 &raquo; Blog Archive &raquo; Drizzle&#8217;s slave plugin
    is working!
  author_email: ''
  author_url: http://www.wc220.com/?p=135
  date: '2011-02-24 09:51:31 +0000'
  date_gmt: '2011-02-24 17:51:31 +0000'
  content: '[...] are exciting times for the Drizzle team.  We just released our first
    RC and things are finally coming together into some awesome new features.  I&#8217;m
    excited to bring [...] '
---
Drizzle source tarball, version 2011.02.10 has been released.  We are proud to announce that this is an RC (Release Candidate) release.

With a GA not too far away, we would greatly appreciate folks helping us to check things out.  We have been working hard on a <a href="http://www.linuxjedi.co.uk/?p=90">number of features</a> and would like your feedback.
<ul>
	<li>New BOOLEAN type</li>
	<li>New UUID type</li>
	<li>Drizzle can now <a href="https://bugs.launchpad.net/drizzle/+bug/565053">fork to background</a> via --daemon: https://bugs.launchpad.net/drizzle/+bug/565053</li>
	<li>NOTE: <a href="https://bugs.launchpad.net/drizzle/+bug/713347">Cartesian joins no longer work</a> and will throw an error - this might break some applications.  They can still be achieved via CROSS JOIN or through the use of a WHERE clause.  It was put in place to prevent runaway queries.</li>
	<li> Innodb transaction log is working and tested.  Both the file-based and innodb-based logs are available.</li>
	<li> Continued work on system variables</li>
	<li> Removed lint.am file as Drizzle no longer uses it</li>
	<li> Continued work on catalogs</li>
	<li> Continued work on our documentation</li>
	<li> Various other bug fixes</li>
</ul>
The Drizzle download file can be found <a href="https://launchpad.net/drizzle/elliott/2011-02-14">here</a>
