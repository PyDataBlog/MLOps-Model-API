# tuxedoneko

tuxedoneko is a minimal blog theme for [hugo](http://gohugo.io).
The theme is based on [Hemingway2](https://github.com/beli3ver/hemingway2.git).

## Getting Started

Clone this repository to your hugo theme directory.

```
mkdir themes
cd themes
git clone https://github.com/tuxedocat/tuxedoneko.git
```

## Configuration

Take a look in the exampleSite dir.

This directory contains an example config file and the content for the demo.
It serves as an example setup for your documentation.

Copy the `config.toml` in the root directory of your website. Overwrite the existing config file if necessary.


```toml
baseurl = "https://example.com"
languageCode = "en"
title = "miaulog"
theme = "tuxedoneko"
copyright = "&copy; tuxedocat"
disqusShortname = "shortname"
googleAnalytics = "trackingcode"

[taxonomies]
tag = "tags"
category = "categories"

[params]
description = "Your description here"
keywords = ["keyword 1", "keyword 2", "keyword 3"]
author = "tuxedocat"
sharingicons = true
enableMathJax = false

[params.highlight]
style = "github"
languages = ["scala", "sh", "python", "r", "go", "dockerfile"]

# Navbar on upper-right area, decorated with Fontawesome icons.
[[params.social]]
name = "Me on Github"                   # -> title
url = "https://github.com/tuxedocat"    # -> href
iconName = "logo-octocat"                   # -> icon name

[[params.social]]
url = "https://gitlab.com/tuxedocat"
iconName = "logo"

[[params.social]]
url = "/index.xml"
iconName = "rss_feed"

[[params.socialshare]]
url = "https://linkedin.com/in/_your_name_"
iconName = "logo-linkedin"
```

## Build

### CSS
Build CSS from Sass using gulp:

```bash
$ yarn add gulp
$ yarn add gulp-sass

# Default: just build css from sass file.
$ yarn gulp

# Or run build for each modification.
$ yarn gulp watch
```

### Hugo
```bash
$ hugo server

# Or set theme variable explicitly
$ hugo server -t tuxedoneko

# Or set theme directory explicitly, if this theme is in somewhere/tuxedoneko
$ hugo server --themeDir somewhere/
```

You can go to localhost:1313 and this theme should be visible.

## License

tuxedoneko is licensed under the [MIT License](LICENSE.md).


## Credits
tuxedoneko is based on Hucore and Hemingway (1 and 2).
