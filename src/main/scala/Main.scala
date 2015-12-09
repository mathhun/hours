case class Summary(val emails: Seq[Email]) {
  var tasks: Seq[Task] = Nil

  def summarize: Summary = {
    this.tasks = this.emails.flatMap(e => Summary.extractTasks(e.body))
    this
  }

  def report: Unit = {
    //println(this.tasks)
  }
}

object Summary {
  def extractTasks(body: Seq[String]): Seq[Task] = {
    val chunk = body
      .dropWhile(!_.matches(""".*\[チケット作業\].*"""))
      .takeWhile(!_.matches("---+"))

    val (tasks, rest) = chunk
      .filter(ln => !ln.matches(""".*\[チケット外?作業\].*"""))
      .partition(ln => ln.matches("""(#[0-9]+)? *(.*) *\([.0-9]+h\).*"""))

    println("**********chunk***********")
    println(chunk.mkString(","))

    println("**********tasks***********")
    println(tasks.mkString(","))

    println("**********rest***********")
    println(rest.mkString(","))

    Seq(Task("", 0))
  }
}

case class Task(name: String, hours: Double, ticketId: Int = 0) {
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
    Summary(emails).summarize.report
  }
}
