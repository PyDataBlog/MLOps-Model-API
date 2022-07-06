/**
 * openircd, a lightweight ircd written in javascript v8 with nodejs.
 * http://www.openbrasil.org/ - rede do conhecimento livre.
 * 
 * $Id$
 */

exports.listen = {
	port: 6667,
	host: '0.0.0.0'
};

exports.server = {
	name: "experimental.openbrasil.org",
	description: "servidor experimental openbrasil",
};

exports.network = {
	name: "openbrasil"
};

exports.general = {
	ping_timeout: 1
};
