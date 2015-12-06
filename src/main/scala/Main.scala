class EmailCollection(val emails: Iterable[Email] = Nil) extends Iterable[Email] {
  override def iterator = emails.iterator

  override def filter(p: Email => Boolean): EmailCollection = {
    new EmailCollection(emails.filter(p))
  }

  def summarize(): Summary = {
    new Summary
  }
}

class Email(val headers: Map[String, String], val body: Seq[String]) {
  val subject = headers("subject")

  def year: Int = ???
  def month: Int = ???
}
object Email {
  def extractYearMonth(s: String): Option[(Int, Int)] = {
    """\d{4}-\d{2}-\{2}""".r.findFirstIn(s) match {
      case _ => None
    }
  }

  def apply(headers: Map[String, String], body: Seq[String]) =
    new Email(headers, body)
}

class Summary {
  def report: Unit = {
  }
}

object Parser {
  def parse(file: String): EmailCollection = {
    new EmailCollection()
  }

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

  def parseEmail(s: String): Email = {
    val (headers, body) = """\n""".r.split(s).span(line => line != "")
    Email(parseHeaders(headers.mkString("\n")), body.tail)
  }
}

object Main {
  def main(args: Array[String]) = {
    val file = args(0)
    val month = args(1).toInt

    val emails = Parser.parse(file)
    emails.filter(e => e.year == 2015 && e.month == month).summarize.report
  }
}
