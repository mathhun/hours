case class Summary(tasks: Seq[Task]) {
  def report: Unit = {
    println("\n// collected tasks")
    tasks.foreach { task => println(task) }

    println("\n// tickets")
    val ticketsSummary = tasks
      .collect { case x@(_: TicketTask) => x }
      .groupBy(_.ticketId)
      .map { case (ticketId, tasks) =>
        TicketTask(tasks.head.name, tasks.map(_.hours).sum, tasks.head.ticketId) }
      .toList.sortBy(_.ticketId)
      .foreach { task => println(task) }
    
    println("\n// misc")
    val miscSummary = tasks
      .collect { case x@(_: MiscTask) => x }
      .groupBy(_.name)
      .map { case (name, tasks) =>
        MiscTask(name, tasks.map(_.hours).sum) }
      .toList.sortBy(_.name)
      .foreach { task => println(task) }

    println("\n// uncategorized")
    val unknown = tasks
      .collect { case x@(_: UnknownTask) => x }
      .foreach { task => println(task) }
  }
}

sealed abstract class Task(name: String) {
  override def equals(that: Any): Boolean
}

case class TicketTask(name: String, hours: Double, ticketId: Int) extends Task(name) {
  override def equals(that: Any): Boolean = this match {
    case t: TicketTask => this.ticketId == t.ticketId
    case _ => false
  }
  override def toString: String = "#%s,\t%s,\t%.1f".format(ticketId, name, hours)
}

case class MiscTask(name: String, hours: Double) extends Task(name) {
  override def equals(that: Any): Boolean = this match {
    case t: MiscTask => this.name == t.name
    case _ => false
  }
  override def toString: String = "%s,\t%.1f".format(name, hours)
}

case class UnknownTask(name: String) extends Task(name) {
  override def equals(that: Any): Boolean = false
  override def toString: String = "??? %s".format(name)
}

object Task {
  val ticket = """#([0-9]+)\s*(.*)\s*\(([.0-9]+)h\).*""".r
  val misc   = """・?\s*(.*)\s*\(([.0-9]+)h\).*""".r

  def extract(emails: Seq[Email]): Summary = {
    Summary(emails.flatMap(e => this.extractFromBody(e.body)))
  }

  def extractFromBody(body: Seq[String]): Seq[Task] = {
    body
      .dropWhile(!_.matches(""".*\[チケット作業\].*"""))
      .takeWhile(!_.matches("---+"))
      .filter(ln => !ln.matches(""".*\[チケット外?作業\].*"""))
      .map(ln => ln match {
        case ticket(ticketId, name, hours) => Some(TicketTask(name.trim, hours.toDouble, ticketId.toInt))
        case misc(name, hours) => Some(MiscTask(name.trim, hours.toDouble))
        case l if l.length > 0 => Some(UnknownTask(ln.trim)) 
        case _ => None
      }).flatten
  }
}

case class Email(headers: Map[String, String], body: Seq[String]) {
  import java.util.{Date, Locale}

  val subject = headers("subject")
  val date: Date = new Date(headers("date"))
  val ymd = Email.extractYearMonth(subject)

  def year: Int = ymd.getOrElse('y, -1)
  def month: Int = ymd.getOrElse('m, -1)
  def day: Int = ymd.getOrElse('d, -1)

  override def toString: String =
    "<Email subject='%s', date='%tF(%ta)'>".format(this.subject, this.date, this.date)
}

object Email {
  def extractYearMonth(s: String): Map[Symbol, Int] = {
    val regex = """.*(\d\d\d\d)-(\d\d)-(\d\d).*""".r
    s match {
      case regex(y, m, d) => Map('y -> y.toInt, 'm -> m.toInt, 'd -> d.toInt)
      case _ => Map.empty
    }
  }
}

object Parser {
  def split(s: String): Seq[String] =
    """(?m)^\.$""".r.split(s).map(_.trim)

  def parseHeaders(content: String): Map[String, String] = {
    import java.io._
    import java.util.Properties
    import javax.mail._
    import javax.mail.internet._

    val s: Session = Session.getDefaultInstance(new Properties())
    val is: InputStream = new ByteArrayInputStream(content.getBytes())
    val message: MimeMessage = new MimeMessage(s, is)
    val e = message.getAllHeaders()
    val m = collection.mutable.Map.empty[String, String]
    while (e.hasMoreElements()) {
      val h: Header = e.nextElement().asInstanceOf[Header]
      m += (h.getName.toLowerCase -> MimeUtility.decodeText(h.getValue))
    }

    m.toMap
  }

  def parseEmail(s: String): Option[Email] = {
    val (headers, body) = """\r\n""".r.split(s).span(line => line != "")
    if (headers.length == 0) None
    else Some(Email(parseHeaders(headers.mkString("\n")), body.tail))
  }

  def parse(file: String): Seq[Email] = {
    import java.io._
    import org.apache.commons.io.FileUtils

    val content = FileUtils.readFileToString(new File(file), "ISO-2022-JP")
    split(content).map(parseEmail).flatten
  }
}

object Main {
  def main(args: Array[String]) = {
    val file = args(0)
    val month = args(1).toInt

    val emails = Parser.parse(file).filter(e => e.year == 2015 && e.month == month)

    println("// processing"); 
    emails.foreach { e => println(e.subject) }

    Task.extract(emails).report
  }
}
