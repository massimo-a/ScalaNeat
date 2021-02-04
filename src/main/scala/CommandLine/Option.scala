package CommandLine

/**
 * Command line option.
 * @author Massimo Angelillo
 * @param name Option name. The standard is to prefix it with --
 * @param shortName Shorthand option name.
 *                  The standard is for it to be a single letter prefixed by -
 * @param helpText Help text.
 * @param values Value passed to the option. An empty string when no value is present.
 * @param isRequired Whether the option is required or not.
 */
case class Option
(name: String,
 shortName: String,
 helpText: String,
 values: List[String] = List(),
 numberOfArguments: Int = 1,
 isRequired: Boolean = false)