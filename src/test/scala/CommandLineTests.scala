import CommandLine.{Option, Parser}
import org.scalatest.FunSuite

class CommandLineTests extends FunSuite {
  val opts = List(
    Option("--option1", "", "testing parsing options", isRequired = true),
    Option("--option2", "", "testing parsing options 2", isRequired = true),
    Option("--value", "-v", "a value"))

  val args = List("--value", "5", "--option1", "hello world", "--option1", "goodbye world", "--invalid", "7")
  val p: Parser = Parser(opts).parse(args)

  /**
   * Tests that all valid options are parsed.
   */
  test("CommandLineTests.validOption") {
    p.withParsed(x => {
      x.name match {
        case "--value" => assert(x.values == List("5"))
        case "--option1" => assert(x.values == List("hello world"))
        case _ => fail()
      }
    })
  }

  /**
   * Tests that invalid options are not parsed and added to errors.
   */
  test("CommandLineTests.invalidOption") {
    p.withErrors(s => {
      if(s.message.indexOf("is an unknown option") > 0) {
        assert(s.message.indexOf("--invalid") > 0)
      }
    })
  }

  /**
   * Tests that duplicate options are not parsed and added to errors.
   */
  test("CommandLineTests.duplicateOptions") {
    p.withErrors(s => {
      if(s.message.indexOf("used multiple times") > 0) {
        assert(s.option == Option("--option1", "", "testing parsing options", isRequired = true))
      }
    })
  }

  /**
   * Tests that a missing required option is added to errors.
   */
  test("CommandLineTests.missingRequired") {
    p.withErrors(s => {
      if(s.message.indexOf("is required") > 0) {
        assert(s.option == Option("--option2", "", "testing parsing options 2", isRequired = true))
      }
    })
  }
}
