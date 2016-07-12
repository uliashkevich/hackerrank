package search.natfirewall

import scala.collection._

object Solution {
  // Host location enum
  abstract sealed class HostLocation
  case object LocalHost extends HostLocation
  case object ExternalHost extends HostLocation
  case object RouterHost extends HostLocation

  // Basic definitions
  case class IP(b1: Int, b2: Int, b3: Int, b4: Int) {
    override def toString = s"$b1.$b2.$b3.$b4"
  }
  case class Address(ip: IP, port: Int) {
    override def toString = s"$ip:$port"
  }
  case class Connection(from: Address, to: Address)

  // Router logging events
  abstract sealed class Event
  case class LocalForwarded(sourceAddress: Address, destAddress: Address) extends Event {
    override def toString = s"$sourceAddress $destAddress local"
  }
  case class PortMapped(sourceAddress: Address, destAddress: Address, forwardingPort: Int) extends Event {
    override def toString = s"Mapped $sourceAddress to $destAddress assigned $forwardingPort"
  }
  case class PacketRejected(sourceAddress: Address, destAddress: Address) extends Event {
    override def toString = s"Reject $sourceAddress $destAddress"
  }
  case class PacketRedirected(sourceAddress: Address, destAddress: Address, redirectedTo: Address) extends Event {
    override def toString = s"Accept $sourceAddress $destAddress redirect $redirectedTo"
  }

  // Firewall state machine class
  class Firewall {
    val RouterIP = IP(53, 0, 0, 1)
    val StartPort = 1025
    val MaxPort = 65535
    var freePort = StartPort
    val portMappings = mutable.HashMap.empty[Connection, Int]
    val backwardMappings = mutable.HashMap.empty[Int, Connection]

    // Determine host location
    def hostLocation(address: Address): HostLocation = {
      address.ip match {
        case RouterIP        => RouterHost
        case IP(10, _, _, _) => LocalHost
        case _               => ExternalHost
      }
    }

    // Process packet
    // All logic resides inside
    // Could be refactored into several methods, but it is quite compact and easy to understand
    def process(sourceAddress: Address, destAddress: Address): Iterable[Event] = {
      val actions = mutable.Buffer.empty[Event]

      (hostLocation(sourceAddress), hostLocation(destAddress)) match {
        case (LocalHost, RouterHost) if destAddress.port < StartPort =>
        case (LocalHost, LocalHost) =>
          actions += LocalForwarded(sourceAddress, destAddress)
        case (LocalHost, ExternalHost) =>
          val connection = Connection(sourceAddress, destAddress)
          if (!portMappings.contains(connection)) {
            portMappings += ((connection, freePort))
            backwardMappings += ((freePort, connection))
            actions += PortMapped(sourceAddress, destAddress, freePort)
            freePort += 1
          }
        case (ExternalHost, RouterHost) =>
          backwardMappings.get(destAddress.port) match {
            case Some(Connection(from, to)) if sourceAddress == to =>
              actions += PacketRedirected(sourceAddress, destAddress, from)
            case _ =>
              actions += PacketRejected(sourceAddress, destAddress)
          }
        case _ =>
          actions += PacketRejected(sourceAddress, destAddress)
      }

      actions
    }
  }

  // Parsing, running router and printing
  def solve() {
    val firewall = new Firewall

    def parseAddress(ip: String, port: String): Address = {
      val bytes = ip.split("\\.").map(_.toInt)
      Address(IP(bytes(0), bytes(1), bytes(2), bytes(3)), port.toInt)
    }

    while (true) {
      val line = readLine()
      if (line == null) {
        return
      }
      val input = line.split(" ")
      val (sourceIP, sourcePort, destIP, destPort) = (input(0), input(1), input(2), input(3))
      val actions = firewall.process(parseAddress(sourceIP, sourcePort), parseAddress(destIP, destPort))
      actions.foreach(println(_))
    }
  }

  def main(args: Array[String]): Unit = {
    solve()
  }
}
