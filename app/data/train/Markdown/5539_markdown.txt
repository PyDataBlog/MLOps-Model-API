---
layout: post
title:  "A Tale of One Thousand Instances - Migrating from EC2-classic to EC2-VPC"
date:   2014-11-12
author: "Andrew Kaczorek"
---

*No slides*

Donald Sumbry, Twilio  
Jonas Borjesson, Twilio  

* REST-based API for voice, sms (SIP)
* When things were simple: single service - PTSN to HTTP
* Brazil, Ireland, Virginia
* traffic has to go through known endpoints. makes it difficult to debug and easy to have routing problems
* Have to do lots between firewalls. lots of holes poked.  Using a “VPN” to send secure traffic between regions. Endpoints have to have elastic IPs
* VPC: global routing tables, elastic network interfaces, software defined network, hardware security manager
* SR-IOV on the high end instances will give higher level of performance
* EIPs take minutes to move (sometimes) and ENIs move in a second or two
* Classic EC2 had problems where private IPs overlapped in different regions
* HIPPA, PCI require data to be protected in transit at all times. Old EC2 meant IPSEC tunnel between every instance. New routing can be encrypted point-to-point in the network
* Twilio uses Ansible to provision the whole network of their VPC
* Twilio in 8 regions (realms)
* Region to region connectivity. They use a router instance on each side
* Much easier call flow. A single global network means global service discovery
* Moving classic->VPC equivalent to moving a datacenter
* had to design their own bridging of a classic to VPC network infrastructure
* vendors for software routers for peering traffic
* service discovery-> zookeeper
* BGP hub&spoke model.  They are doing their routing from the edges of their network
