package vexriscv.demo.bridge

import spinal.lib._
import spinal.core._
import spinal.core.fiber._
import spinal.lib.generator._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.misc.Clint
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbParameter, BmbSlaveFactory,BmbImplicitPeripheralDecoder, BmbInterconnectGenerator}
import spinal.lib.bus.misc.BusSlaveFactory

import scala.collection.Seq

// import spinal.core._
// import spinal.core.fiber.{Fiber, Lock}
// import spinal.lib._
// import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
// import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SlaveFactory}
// import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig, WishboneSlaveFactory}
// import spinal.lib.cpu.riscv.RiscvHart
// import scala.collection.Seq


case class ClintMultiPorts(hartIds : Seq[Int]) extends Area {
  val stop = False
  val time = Reg(UInt(64 bits)) init(0)
  when(!stop){
    time := time + 1
  }

  val harts = for(hartId <- hartIds) yield new Area{
    val cmp = Reg(UInt(64 bits))
    val timerInterrupt = RegNext(time >= cmp)
    val softwareInterrupt = RegInit(False)
  }

  def driveFrom(buses : Seq[BusSlaveFactory], bufferTime : Boolean = false) = new Area{
    val IPI_ADDR = 0x0000
    val CMP_ADDR = 0x4000
    val TIME_ADDR = 0xBFF8

    buses.foreach { bus => {
      bufferTime match {
        case false => bus.readMultiWord(time, TIME_ADDR)
        case true => new Composite(this){
          assert(bus.busDataWidth == 32)

          val timeMsb = RegNextWhen(time(63 downto 32), bus.isReading(TIME_ADDR))
          bus.read(time(31 downto 0), TIME_ADDR)
          bus.read(timeMsb, TIME_ADDR + 4)
        }
      }
    }}

    if (buses.size != hartIds.size) throw new RuntimeException("Invalid ClintMultiPorts Instance")

    val hartsMapping = buses.zip(hartIds).map{ case (bus, hartId) => new Area {
      bus.writeMultiWord(harts(hartId).cmp, CMP_ADDR + 8*hartId)
      bus.readAndWrite(harts(hartId).softwareInterrupt, IPI_ADDR + 4*hartId, bitOffset = 0)
    }}
  }

}


case class BridgeClint(bmbParameter : BmbParameter, hartCount : Int) extends Component{
  val io = new Bundle {
    val buses = (0 until hartCount).map(_ => slave(Bmb(bmbParameter)))
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
    val stop = in Bool() default(False)
  }

  val factories = io.buses.map(BmbSlaveFactory(_))
  val logic = ClintMultiPorts(0 until hartCount)
  logic.driveFrom(factories)
  logic.stop setWhen(io.stop)

  (0 until hartCount).foreach{ hartId => {
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }}

  io.time := logic.time
}


case class BridgeClintGenerator(apbOffset : Handle[BigInt] = Unset)
                            (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area {
  val ctrl = Handle(logic.io.buses)
  val stop = Handle(logic.io.stop)
  val cpuCount = Handle[Int]

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]
  val logic = Handle(BridgeClint(accessRequirements.toBmbParameter(), cpuCount))
  def timerInterrupt(id : Int) = logic.derivate(_.io.timerInterrupt(id))
  def softwareInterrupt(id : Int) = logic.derivate(_.io.softwareInterrupt(id))

  if(interconnect != null) {
    ctrl.map(c => {
      c.foreach { b => interconnect.addSlave(
        accessSource = accessSource,
        accessCapabilities = accessSource.derivate(Clint.getBmbCapabilities),
        accessRequirements = accessRequirements,
        bus = b,
        mapping = apbOffset.derivate(SizeMapping(_, 1 << Clint.addressWidth))
      )}
    })
  }

  val hz = sexport(Handle(ClockDomain.current.frequency))
  if(decoder != null) {
    ctrl.map(c => c.foreach(b => interconnect.addConnection(decoder.bus, b)))
  }
}
