package exceptions

case class TypeMismatchException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
