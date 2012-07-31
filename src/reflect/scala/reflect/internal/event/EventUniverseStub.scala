package scala.reflect
package internal
package event

trait EventUniverseStub {
  outer: SymbolTable =>

  val EV = new EventModel {
    // Some of the stuff would need to have to be moved to reflect.interal to correctly initialise EV
    val global: outer.type = outer
    type EventResponse = Unit
    type CompilationUnit = NoCompilationUnit.type
    type Phase = internal.Phase

    def <<(ev: Event): Unit =  {}
    def >>(ev: Event): Unit =  {}
    def >>>(ev: Event): Unit = {}
    def <<<(ev: Event): Unit = {}
    val Filter: FilterCompanion = null
    val Hook: HookCompanion = null
    val NameManifest: scala.reflect.Manifest[outer.Name] = null
    val NoPhase: Phase = SomePhase
    val NoResponse: EventResponse = ()
    val PositionManifest: scala.reflect.Manifest[outer.Position] = manifest[Position]
    val SymbolManifest: scala.reflect.Manifest[outer.Symbol] = manifest[Symbol]
    val TreeManifest: scala.reflect.Manifest[outer.Tree] = manifest[Tree]
    val TypeManifest: scala.reflect.Manifest[outer.Type] = manifest[Type]
    def anyString(x: Any): String = ""
    def currentPhase = phase
    def currentPos: outer.Position = outer.NoPosition
    def currentUnit: CompilationUnit = NoCompilationUnit
    def currentFile: Option[AbstractScalaFile] = None
    def eventsOn: Boolean = false
    def flagsString(flags: Long): String = internal.Flags.flagsToString(flags)
    def posString(pos: outer.Position): String = ""
    def formatTypeString(tpe: Type): String = "(not implemented)"
    def eventHooks: List[Hook] = List()
    def openBlockResponse(ev: Event): EventResponse = NoResponse
    def instrumentingOn = false

    object NoCompilationUnit {
      def source: Any = null
    }
    
    object NoAbstractType {
      def file: java.io.File = null
    }
  }
  
  // clocks stub
  def newClockTick(): Clock = null
  def currentClock(): Clock = null
  def isClockOn: Boolean = false
}