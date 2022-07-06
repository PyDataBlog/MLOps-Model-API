package smtp

import (
	"net/smtp"

	qlang "qlang.io/spec"
)

// Exports is the export table of this module.
//
var Exports = map[string]interface{}{
	"_name": "net/smtp",

	"sendMail": smtp.SendMail,

	"CRAMMD5Auth": smtp.CRAMMD5Auth,
	"plainAuth":   smtp.PlainAuth,

	"Client":     qlang.StructOf((*smtp.Client)(nil)),
	"client":     smtp.NewClient,
	"dial":       smtp.Dial,
	"ServerInfo": qlang.StructOf((*smtp.ServerInfo)(nil)),
}
