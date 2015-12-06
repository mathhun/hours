import org.scalatest._

class ParserSpec extends FunSpec with Matchers {
  describe("Parser.split") {
    it("should split string with a dot") {
      Parser.split("line one.\n.\nline. two.\n") should be (Seq("line one.", "line. two."))
    }
  }

  val emailString =
"""Return-Path: <email@example.com>
X-Original-To: email@example.com
Delivered-To: email@example.com
Date: Tue, 01 Sep 2015 18:54:36 +0900
Subject: =?ISO-2022-JP?B?GyRCJUclMyE8JUkkNyRGJE0bKEI=?=
X-Mailer: Becky! ver. 2.69 [ja]

email body 00
email body 01
"""
  describe("Parser.parseEmail") {
    it("should parse a string and return an email object") {
      val email = Parser.parseEmail(emailString)
      email.body should be (Seq("email body 00", "email body 01"))
    }
  }

  describe("Parser.parseHeaders") {
    val h = Parser.parseHeaders(emailString)
    h("subject") should be ("デコードしてね")
  }
}
