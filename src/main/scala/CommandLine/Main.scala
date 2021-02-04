package CommandLine

object Main {
  def main(args: Array[String]): Unit = {
    val options = List(
      Option("--add", "-a", "add two comma-separated numbers", numberOfArguments = 2),
      Option("--sub", "-s", "subtract two comma-separated numbers", numberOfArguments = 2),
      Option("--mult", "-m", "multiply two comma-separated numbers", numberOfArguments = 2),
      Option("--div", "-d", "divide two comma-separated numbers", numberOfArguments = 2)
    )

    val args = List("-a", "100", "101", "-s", "20", "10", "-m", "5", "12", "-d", "4", "2")
    val p: Parser = Parser(options).parse(args)

    val parsed = p.withParsed(opt => {
      val valuesToInts = opt.values.map(x => x.toInt)
      opt.shortName match {
        case "-a" => valuesToInts.sum
        case "-s" => valuesToInts.reduce((a, b) => a - b)
        case "-m" => valuesToInts.product
        case "-d" => valuesToInts.reduce((a, b) => a / b)
      }
    })

    println(parsed)
  }
}
