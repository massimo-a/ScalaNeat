package CommandLine

/**
 * Parser for list of command line arguments.
 * @author Massimo Angelillo
 * @param options List of available options.
 * @param values Map with the option name as the key and the option, with its value, as the value
 * @param errors Any errors that occurred while parsing
 */
case class Parser
(options: List[Option],
 values: Map[String, Option] = Map(),
 errors: List[Error] = List()) {
  def withParsed[T](f: Option => T): List[T] = {
    values.map(x => f(x._2)).toList
  }

  def withErrors[T](f: Error => T): List[T] = {
    errors.map(f)
  }

  def parse(args: List[String]): Parser = {
    parse(args, List(), List(), Map())
  }

  def usage(): String = {
    val maxLenName = options.maxBy(x => x.name.length).name.length
    options.map(x => {
      val padding = " "*(maxLenName - x.name.length)
      s"${x.name}$padding \t ${x.shortName} \t\t ${x.helpText} \t ${if(x.isRequired) "required" else ""}"
    }).fold("Usage :")((prev, curr) => {
      prev + System.lineSeparator() + curr
    }) + System.lineSeparator()
  }

  @scala.annotation.tailrec
  private def parse(args: List[String], required: List[Option], errors: List[Error], accu: Map[String, Option]): Parser = {
    if(args.isEmpty) {
      return Parser(options, accu, addMissingOptionErrors(required))
    }
    val v = options.find(x => x.name == args.head || x.shortName == args.head)
    v match {
      case None =>
        val errs = errors.prepended(Error(s"${args.head} is an unknown option", null))
        parse(args.drop(1), required, errs, accu)
      case Some(opt) =>
        if(accu.contains(opt.name)) {
          val errs = errors.prepended(Error(s"${args.head} used multiple times", opt))
          parse(args.drop(1 + opt.numberOfArguments), required, errs, accu)
        } else {
          val accu2 = accu + (opt.name -> opt.copy(values = args.slice(1, opt.numberOfArguments + 1)))
          val r = if(opt.isRequired) required.prepended(opt) else required
          parse(args.drop(1 + opt.numberOfArguments), r, errors, accu2)
        }
    }
  }

  private def addMissingOptionErrors(required: List[Option]): List[Error] = {
    val missingRequired = options.filter(x => x.isRequired) diff required
    if(missingRequired.nonEmpty) {
      errors.prependedAll(missingRequired.map(x => Error(s"${x.name} is required", x)))
    } else {
      errors
    }
  }
}
