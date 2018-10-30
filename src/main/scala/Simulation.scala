package scala

abstract class Simulation {
  case class Event(time: Int, action: Action)

  type Action = () => Unit

  def currentTime: Int =  curTime

  private var curTime = 0

  private var agenda: List[Event] = List()

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest  if first.time <= item.time => first :: insert(rest, item)
    case _ => item :: ag
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case _ =>
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    afterDelay(0){
      println("*** simulation started, time = " + currentTime + " ***")
    }

    loop()
  }
}
