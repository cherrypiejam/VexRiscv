package vexriscv.demo.bridge

import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.generator._
import spinal.lib._
import spinal.core._
import spinal.core.fiber._
import spinal.lib.misc.plic.{PlicGateway, PlicGatewayActiveHigh, PlicMapper, PlicMapping, PlicTarget}
import spinal.lib.bus.bmb._

import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq


case class BridgePlicGenerator(apbOffset : Handle[BigInt] = Unset) (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area with InterruptCtrlGeneratorI{
  @dontName val gateways = ArrayBuffer[Handle[PlicGateway]]()
  val ctrl = Handle(logic.bmb)
  val contextCtrls = Handle(logic.contextBmbs.map(c => Handle(c)))

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]

  val priorityWidth = Handle[Int]
  val mapping = Handle[PlicMapping]

  val lock = Lock()

  case class TargetModel(target : Handle[Bool], clockDomain : Handle[ClockDomain])
  val targetsModel = ArrayBuffer[TargetModel]()
  def addTarget(target : Handle[Bool]) = {
    val id = targetsModel.size
    targetsModel += TargetModel(target, ClockDomain.currentHandle)

    //TODO remove the need of delaying stuff for name capture
    Handle(Component.current.addTag(new Export(BridgePlicGenerator.this.getName() + "_" + target.getName, id)))
  }

  override def addInterrupt(source : => Handle[Bool], id : Int) = {
    lock.retain()
    Handle{
      val src = source
      soon(lock)
      gateways += PlicGatewayActiveHigh(
        source = src,
        id = id,
        priorityWidth = priorityWidth
      ).setCompositeName(src, "plic_gateway")

      Component.current.addTag (new Export(BridgePlicGenerator.this.getName() + "_" + src.getName, id))
      lock.release()
    }
  }

  override def getBus(): Handle[Nameable] = ctrl

  val logic = Handle(new Area{
    lock.await()

    // NOTE: this assumes targets == # of CPU cores
    val bmb = Bmb(accessRequirements.toBmbParameter())
    bmb.setName("bmbbus123")
    val bus = BmbSlaveFactory(bmb)
    val contextBmbs = targetsModel.map(_ => Bmb(accessRequirements.toBmbParameter()))
    for ((c, i) <- contextBmbs.zipWithIndex) {
      c.setName(f"contextbmbmb$i")
    }
    val contextBuses = contextBmbs.map(bmb => BmbSlaveFactory(bmb))

    val targets = (targetsModel.zipWithIndex).map { case (flag, id) =>
      PlicTarget(
        id = id,
        gateways = gateways.map(_.get),
        priorityWidth = priorityWidth
      ).setCompositeName(flag.target, "plic_target")
    }

    //    gateways.foreach(_.priority := 1)
    //    targets.foreach(_.threshold := 0)
    //    targets.foreach(_.ie.foreach(_ := True))

    val bridge = BridgePlicMapper(bus, contextBuses, mapping)(
      gateways = gateways.map(_.get),
      targets = targets
    )

    for(targetId <- 0 until targetsModel.length){
      val plicCd = ClockDomain.currentHandle
      def bufferize[T <: Data](that : T) : T = if(targetsModel(targetId).clockDomain != ClockDomain.currentHandle) targetsModel(targetId).clockDomain on BufferCC[T](plicCd on RegNext(that), init = null.asInstanceOf[T]) else RegNext[T](that)
      targetsModel(targetId).target := bufferize(targets(targetId).iep)
    }
  })


  if(interconnect != null) interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = Handle(BmbSlaveFactory.getBmbCapabilities(
      accessSource,
      addressWidth = 22,
      dataWidth = 32
    )),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = Handle(SizeMapping(apbOffset, 1 << 22))
  )

  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, AllMapping, SingleMapping}

import scala.collection.Seq

object BridgePlicMapper{
  // args for PlicMapper:
  // bus: bus to which this ctrl is attached
  // mapping: a mapping configuration (see above)
  // gateways: a sequence of PlicGateway (interrupt sources) to generate the bus access control
  // targets: the sequence of PlicTargets (eg. multiple cores) to generate the bus access control
  // def apply(bus: BusSlaveFactory, contextBuses: Seq[BusSlaveFactory], mapping: PlicMapping)(gateways : Seq[PlicGateway], targets : Seq[PlicTarget]) = new Area{
  def apply
    (bus: BusSlaveFactory, contextBuses: Seq[BusSlaveFactory], mapping: PlicMapping)
    (gateways : Seq[PlicGateway], targets : Seq[PlicTarget])
  = new Area {
    import mapping._

    val selectedState = new SpinalEnum{
      val CLAIM, COMPLETION = newElement()
    }

    // Why coherency?
    val coherencyLogic = (bus +: contextBuses).map {
      b => new Area {
        val coherencyStall = Counter(3)
        when(coherencyStall =/= 0){
          b.readHalt()
          coherencyStall.increment()
        }
        b.onReadPrimitive(AllMapping, haltSensitive = false, documentation = ""){
          coherencyStall.increment()
        }
        b.onWritePrimitive(AllMapping, haltSensitive = false, documentation = ""){
          coherencyStall.increment()
        }
      }
    }

    // Generate threshold and claim/complete registers for each target
    val targetsLogic = for ((target, contextBus) <- targets.zip(contextBuses)) yield new Area {
      val thresholdOffset = targetThresholdOffset + (target.id << targetThresholdShift)
      val claimOffset = targetClaimOffset + (target.id << targetClaimShift)
      if (targetThresholdWriteGen && !target.threshold.hasAssignement)
        contextBus.drive(
          target.threshold,
          address = thresholdOffset,
          documentation = s"Drive target threshold for target ${target.id}. inits to 0"
        ) init (0)
      if (targetThresholdReadGen)
        contextBus.read(
          target.threshold,
          address = thresholdOffset,
          documentation = s"Read target threshold for target ${target.id}"
        )
      contextBus.read(
        target.claim,
        address = claimOffset,
        documentation = s"Read target claim for target ${target.id} "
      )

      val selected = Stream(selectedState())
      val select = Stream(UInt(target.idWidth bits))
      selected.valid := False
      selected.payload.assignDontCare()
      select.valid := False
      select.payload.assignDontCare()

      // ************************** //
      // Claim logic for the target //
      // ************************** //
      val claimSelect = Stream(UInt(target.idWidth bits))
      claimSelect.valid := False
      claimSelect.payload.assignDontCare()
      claimSelect.ready := False
      // Drive the selector stream on read from the context bus
      contextBus.onRead(claimOffset) {
        selected.valid := True
        // Transate claim id to selector index
        switch (target.claim) {
          for ((gateway, gatewayIndex) <- gateways.zipWithIndex) {
            is (gateway.id) {
              claimSelect.valid := True
              claimSelect.payload := gatewayIndex
            }
          }
        }
      }

      // ******************************* //
      // Completion logic for the target //
      // ******************************* //
      val completionTarget = Stream(UInt(target.idWidth bits))
      val completionSelect = Stream(UInt(target.idWidth bits))
      completionSelect.ready := False
      // Drive the selecter stream from the context bus
      contextBus.driveStream(completionTarget, claimOffset)
      val completionTargetPiped = completionTarget.s2mPipe()
      completionSelect.translateFrom(completionTargetPiped) { (cselect, ctarget) => {
        switch (ctarget) {
          for ((gateway, gatewayIndex) <- gateways.zipWithIndex) {
            is (gateway.id) {
              cselect := gatewayIndex
            }
          }
          default {
            cselect.assignDontCare()
          }
        }
      }}

      when (completionSelect.valid) {
        selected.valid := True
        selected.payload := selectedState.COMPLETION
        select << completionSelect
      } elsewhen (claimSelect.valid) {
        selected.valid := True
        selected.payload := selectedState.CLAIM
        select << claimSelect
      }

      // Generate claim/completion streams for each gateway
      val claimCompletions = StreamDemux.joinSel(selected, select, gateways.size)
      claimCompletions.setCompositeName(select, "claimCompletions")

      // Generate enable bits of each gateway for the target
      for ((gateway, gatewayIndex) <- gateways.zipWithIndex) {
        val address = (targetEnableOffset
          + (target.id << targetEnableShift)
          + contextBus.busDataWidth/8 * (gateway.id / bus.busDataWidth))
        val bitOffset = gateway.id % contextBus.busDataWidth
        if (targetEnableWriteGen && !target.ie(gatewayIndex).hasAssignement)
          contextBus.drive(
            target.ie(gatewayIndex),
            address,
            bitOffset,
            documentation = s"Drive target enable for gateway ${gatewayIndex} for target ${target.id}. inits to 0b0"
          ) init(False)
        if (targetEnableReadGen)
          contextBus.read(
            target.ie(gatewayIndex),
            address,
            bitOffset,
            documentation = s"Read target enable for gateway ${gatewayIndex} for target ${target.id}."
          )
      }
    }

    // Generate logic for each gateway
    val gatewaysClaimCompletions = targetsLogic.map(_.claimCompletions).transpose
    val gatewaysLogic = gateways.zip(gatewaysClaimCompletions).map {
      case (gateway, claimCompletions) => new Area {
        // Generate priority and pending bits for each gateway
        if (gatewayPriorityWriteGen && !gateway.priority.hasAssignement)
          bus.drive(
            gateway.priority,
            address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift),
            documentation = s"Driving priority for gateway ${gateway.getName()}. Inits to 0 (interrupt is disabled)" ) init(0)
        if (gatewayPriorityReadGen)
          bus.read(
            gateway.priority,
            address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift),
            documentation = s"Read priority for gateway ${gateway.getName()}"
          )
        if (gatewayPendingReadGen)
          bus.read(
            gateway.ip,
            address = gatewayPendingOffset + (gateway.id/bus.busDataWidth)*bus.busDataWidth/8,
            bitOffset = gateway.id % bus.busDataWidth,
            documentation = s"Read Pending bit for gateway " + gateway.getName()
          )

        // Generate logic for claim and completion
        val claimCompletionArbitered = StreamArbiterFactory.roundRobin
          .transactionLock.on(claimCompletions)
        claimCompletionArbitered.ready := False
        when(claimCompletionArbitered.valid) {
          switch(claimCompletionArbitered.payload) {
            is(selectedState.CLAIM) {
              gateway.doClaim()
            }
            is(selectedState.COMPLETION) {
              gateway.doCompletion()
            }
            claimCompletionArbitered.ready := True
          }
        }
      }
    }
  }
}

case class TestBmbPlicGenerator(apbOffset : Handle[BigInt] = Unset) (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area with InterruptCtrlGeneratorI{
  @dontName val gateways = ArrayBuffer[Handle[PlicGateway]]()
  val ctrl = Handle(logic.bmb)
  // val contextCtrl0 = Handle(logic.contextBmbs(0))
  // val contextCtrl1 = Handle(logic.contextBmbs(1))
  // def contextCtrls = {
  //   targetsModel.zipWithIndex.map {
  //     case (_, i) => Handle(logic.contextBmbs(i))
  //   }
  // }
  val contextCtrls = Handle(logic.contextBmbs)

  // def targetN = {
  //   targetsModel.size
  // }
  // val contextCtrls = Handle(logic.contextBmbs)

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]

  val priorityWidth = Handle[Int]
  val mapping = Handle[PlicMapping]

  val lock = Lock()

  case class TargetModel(target : Handle[Bool], clockDomain : Handle[ClockDomain])
  val targetsModel = ArrayBuffer[TargetModel]()
  def addTarget(target : Handle[Bool]) = {
    val id = targetsModel.size
    targetsModel += TargetModel(target, ClockDomain.currentHandle)

    //TODO remove the need of delaying stuff for name capture
    Handle(Component.current.addTag(new Export(TestBmbPlicGenerator.this.getName() + "_" + target.getName, id)))
  }

  override def addInterrupt(source : => Handle[Bool], id : Int) = {
    lock.retain()
    Handle{
      val src = source
      soon(lock)
      gateways += PlicGatewayActiveHigh(
        source = src,
        id = id,
        priorityWidth = priorityWidth
      ).setCompositeName(src, "plic_gateway")

      Component.current.addTag (new Export(TestBmbPlicGenerator.this.getName() + "_" + src.getName, id))
      lock.release()
    }
  }

  override def getBus(): Handle[Nameable] = ctrl

  val logic = Handle(new Area{
    lock.await()
    val bmb = Bmb(accessRequirements.toBmbParameter())
    val contextBmbs = List(Bmb(accessRequirements.toBmbParameter()),Bmb(accessRequirements.toBmbParameter()))
    val bus = BmbSlaveFactory(bmb)
    val contextBuses = contextBmbs.map(c=>BmbSlaveFactory(c))
    val targets = (targetsModel.zipWithIndex).map { case (flag, id) =>
      PlicTarget(
        id = id,
        gateways = gateways.map(_.get),
        priorityWidth = priorityWidth
      ).setCompositeName(flag.target, "plic_target")
    }

    //    gateways.foreach(_.priority := 1)
    //    targets.foreach(_.threshold := 0)
    //    targets.foreach(_.ie.foreach(_ := True))

    val bridge = TestPlicMapper(bus, contextBuses, mapping)(
      gateways = gateways.map(_.get),
      targets = targets
    )

    for(targetId <- 0 until targetsModel.length){
      val plicCd = ClockDomain.currentHandle
      def bufferize[T <: Data](that : T) : T = if(targetsModel(targetId).clockDomain != ClockDomain.currentHandle) targetsModel(targetId).clockDomain on BufferCC[T](plicCd on RegNext(that), init = null.asInstanceOf[T]) else RegNext[T](that)
      targetsModel(targetId).target := bufferize(targets(targetId).iep)
    }
  })


  if(interconnect != null) interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = Handle(BmbSlaveFactory.getBmbCapabilities(
      accessSource,
      addressWidth = 22,
      dataWidth = 32
    )),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = Handle(SizeMapping(apbOffset, 1 << 22))
  )

  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}

object TestPlicMapper{
  // args for PlicMapper:
  // bus: bus to which this ctrl is attached
  // mapping: a mapping configuration (see above)
  // gateways: a sequence of PlicGateway (interrupt sources) to generate the bus access control
  // targets: the sequence of PlicTargets (eg. multiple cores) to generate the bus access control
  def apply(bus: BusSlaveFactory, contextBuses: Seq[BusSlaveFactory], mapping: PlicMapping)(gateways : Seq[PlicGateway], targets : Seq[PlicTarget]) = new Area{
    import mapping._
    
    // for each gateway, generate priority register & pending bit as needed
    val gatewayMapping = for(gateway <- gateways) yield new Area{
      if(gatewayPriorityWriteGen && !gateway.priority.hasAssignement) bus.drive(gateway.priority, address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift), documentation = s"Driving priority for gateway ${gateway.getName()}. Inits to 0 (interrupt is disabled)" ) init(0)
      if(gatewayPriorityReadGen) contextBuses(0).read(gateway.priority, address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift), documentation = s"Read priority for gateway ${gateway.getName()}")
      if(gatewayPendingReadGen) contextBuses(1).read(gateway.ip, address = gatewayPendingOffset + (gateway.id/bus.busDataWidth)*bus.busDataWidth/8, bitOffset = gateway.id % bus.busDataWidth, documentation = s"Read Pending bit for gateway " + gateway.getName())
    }

    // claim/complete logic
    val idWidth = log2Up((gateways.map(_.id) ++ Seq(0)).max + 1)
    val claim = Flow(UInt(idWidth bits))
    claim.valid := False
    claim.payload.assignDontCare()
    when(claim.valid) {
      switch(claim.payload) {
        for (gateway <- gateways) {
          is(gateway.id) {
            gateway.doClaim()
          }
        }
      }
    }

    val completion = Flow(UInt(idWidth bits))
    completion.valid := False
    completion.payload.assignDontCare()
    when(completion.valid) {
      switch(completion.payload) {
        for (gateway <- gateways) {
          is(gateway.id) {
            gateway.doCompletion()
          }
        }
      }
    }


    val coherencyStall = Counter(2)
    when(coherencyStall =/= 0){
      bus.readHalt()
      coherencyStall.increment()
    }
    bus.onReadPrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }
    bus.onWritePrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }

    // for each target/context, generate threshold and claim/complete registers
    val targetMapping = for(target <- targets) yield new Area {
      val thresholdOffset = targetThresholdOffset + (target.id << targetThresholdShift)
      val claimOffset = targetClaimOffset + (target.id << targetClaimShift)
      if(targetThresholdWriteGen && !target.threshold.hasAssignement) bus.drive(target.threshold, address = thresholdOffset, documentation = s"Drive target threshold for target ${target.id}. inits to 0") init (0)
      if(targetThresholdReadGen) bus.read(target.threshold, address = thresholdOffset, documentation = s"Read target threshold for target ${target.id}")
      bus.read(target.claim, address = claimOffset, documentation = s"Read target claim for target ${target.id} ")
      bus.onRead(claimOffset) {
        claim.valid := True
        claim.payload := target.claim
      }




      val targetCompletion = bus.createAndDriveFlow(UInt(target.idWidth bits), claimOffset)
      when(targetCompletion.valid){
        completion.valid := True
        completion.payload := targetCompletion.payload
      }
      // for each gateway/interrupt source, generate the enable bits for each target/context
      for ((gateway, gatewayIndex) <- gateways.zipWithIndex) {
        val address = targetEnableOffset + (target.id << targetEnableShift) + bus.busDataWidth/8 * (gateway.id / bus.busDataWidth)
        val bitOffset = gateway.id % bus.busDataWidth
        if(targetEnableWriteGen && !target.ie(gatewayIndex).hasAssignement) bus.drive(target.ie(gatewayIndex), address, bitOffset, documentation = s"Drive target enable for gateway ${gatewayIndex} for target ${target.id}. inits to 0b0") init(False)
        if(targetEnableReadGen)  bus.read(target.ie(gatewayIndex),  address, bitOffset, documentation = s"Read target enable for gateway ${gatewayIndex} for target ${target.id}.")
      }
    }
  }
}
