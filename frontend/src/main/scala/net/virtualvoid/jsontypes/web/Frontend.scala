package net.virtualvoid.jsontypes.web

object Frontend {
  def main(args: Array[String]): Unit = {
    AutoReloader("/ws-watchdog", 200)
  }
}