---
layout: post
title: "Copy DNS from pfSense unbound to windows"
comments: true
date: "2016-11-25"
description: "copy dns from pfsense unbound to windows"
keywords: "dns, pfsense, unbound"
categories: [powershell, tool, script, interactive, reference]
tags: [dns, pfsense, unbound]
---

This script will assist in moving from unbound DNS on pfSense to Windows Server DNS. It parses the backup XML pfSense creates and creates A and CNAME records in the windows DNS zone.
Steps:

1. backup pfSense config to XML.
2. prep zone on windows server (if new)
3. run script into memory, call function **Import-pfSUnDNS** 

see example in comments

{% gist jbilinski/fa810183c09d0d2f9d54d2dd6a00ff13 copy-dns-from-pfsense-unbound-to-windows-1.ps1 %}

Once complete, you can remove all unbound DNS records and redirect/stub to windows DNS servers, or update hosts DNS servers.
