package exceptions

class VariableNotBoundException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
