---
layout: post
title: How/why podify an external dependency ?
categories:
- freelancing
status: publish
type: post
published: true
---


So you may use externals pods and you may have packaged your internal libraries as pods. But how do you handle a `Vendor.framework` ?

It will look like a lot of hassle compared to just drag'n'dropping into Xcode's Project Navigator. But if you use a lot of external dependencies and they are update often, it will save you a lot of time when incompatibilities (or simply the need to iterate through versions of many libs) arise.

It may look dead simple, and that's because ... it is. But as lazy as we are, we can easily think that updating manually is so fast/simple that the podification is not profitable enough.

<a href="http://xkcd.com/1205/">xkcd said it better</a>:

[caption id="attachment_241" align="aligncenter" width="571"]<a href="https://wearemobiledevelopers.files.wordpress.com/2014/11/is_it_worth_the_time.png"><img class="wp-image-241 size-full" src="https://wearemobiledevelopers.files.wordpress.com/2014/11/is_it_worth_the_time.png" alt="is_it_worth_the_time" width="571" height="464" /></a> xckd : http://xkcd.com/1205/[/caption]
<h6>1. Create a pod.</h6>
Really simple. Just drop the whole content of the vendor's stuffs into a git repo, push it somewhere it can be fetched.
<h6>2. Create a podspec.</h6>
Nearly as simple as step 1. The only tricky part is to be sure to keep all the needed files.  Thus inside your "<strong>Pod::Spec.new do |s|</strong>", It will save you some time to have a look at
<ul>
	<li>s.resources</li>
	<li>s.preserve_paths</li>
	<li>s.vendored_frameworks</li>
	<li>s.vendored_libraries</li>
</ul>
<h6>3. Update your podfile</h6>
That's all. Really. And now, you can update wholeheartedly because you will be able to revert.