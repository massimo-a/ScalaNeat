package CommandLine

/**
 * Error when parsing.
 * @author Massimo Angelillo
 * @param message Error message.
 * @param option Option that caused the error. Null if caused by an invalid option.
 */
case class Error(message: String, option: Option)
