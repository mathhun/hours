class EmailCollection(val emails: Iterable[Email] = Nil) extends Iterable[Email] {
  override def iterator = emails.iterator

  override def filter(p: Email => Boolean): EmailCollection = {
    new EmailCollection(emails.filter(p))
  }

  def summarize(): Summary = {
    new Summary
  }
}

class Email(val year: Int, val month: Int, val subject: String, val body: String) {
}

class Summary {
  def report: Unit = {
  }
}

object Parser {
  def parse(file: String): EmailCollection = {
    new EmailCollection()
  }

  def parseEmail(s: String): Email = {
    ???
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
