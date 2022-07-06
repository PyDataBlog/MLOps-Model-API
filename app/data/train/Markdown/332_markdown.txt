---
layout: post
title:  "Hello World"
image:
  feature: nomadsland.jpg
date:   2016-09-19 20:24:11 +0200
categories: personal, coding
---
I enjoy tinkering - building little electronic gadgets and apps - especially if it has something to do with [cryptocurrencies]. Currently my day-job is building Bitcoin infrastructure. The purpose of this weblog is to record the progress on various projects outside my normal scope of work. Working with software, I seem to hit a lot of **"Hmmm, I wonder how this works?"** moments. Hopefully these pages provide some useful information to somebody working on and wondering about the same things.

## Begin
I'm a fan of the maxim "Done is better than perfect" so my current goal is to get this blog up and running as quickly as possible. Instead of wondering about which software or host to use, I decided to power it with [Jekyll] and host it on [Github pages]. I already have a code/text editor I like using and git is already part of my daily workflow so it seemed like a natural fit. I picked a theme I liked but just as I got something that looked vaguely OK, I ran into the first **"Hmmm, I wonder"** moment.

It turns out Jekyll/Github pages deployments are not intuitive. 

[Jekyll's Basic Usage Guide] does a pretty good job of explaining how to get up and running locally, but trying to find GH-Pages deployment instructions (and failing miserably) made me realise things don't work as I'd expect.

## Jekyll & Github
Jekyll uses plugins and templates to help authors perform dynamic blog-related things like themeing, categorising and indexing posts, generating "about" pages and more.

`jekyll build` produces a static "render" of the blog at the time it is called so that you can just upload the HTML/JS/CSS contents of the generated `_site` directory to a server that serves static content. You could host it on dropbox if you wanted to, there's no server-side scripts like PHP or Rails to generate content from data stored in a database.

`_site` gets destroyed and re-created every time you call `jekyll build` and it is listed in `.gitignore` so it's not part of the repo, so how in the world do you tell Github to serve the contents of `_site`? Am I supposed to create a manual deploy script that adds `_site` to a repo and push that?

## Hmmm, I wonder...

So which is it? 

Turns out you simply push your jekyll source repo to `master`. You don't even need to call `jekyll build`. Your site just magically appears on Github. That leaves only two possibilities:

1. Either Github calls `jekyll build` and serves the `_site` directory after each push, or

2. Github is running `jekyll serve` to generate content every request. 

Turns out it's the latter. Github actually runs Jekyll, and supports [some Jekyll plugins].

## Done
So, one "git push" later and these pages should be live! 

*"Done is better than perfect"*

[BitX]: https://www.bitx.co/
[cryptocurrencies]: https://en.wikipedia.org/wiki/Cryptocurrency
[jekyll]: https://jekyllrb.com
[Jekyll's Basic Usage Guide]: https://jekyllrb.com/docs/usage/
[Github pages]: https://pages.github.com/
[some Jekyll plugins]: https://help.github.com/articles/adding-jekyll-plugins-to-a-github-pages-site/