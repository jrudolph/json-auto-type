package net.virtualvoid.jsontypes.web

case class ServiceConfig(
    autoreload: Boolean,
    username:   String  = "",
    password:   String  = ""
)
