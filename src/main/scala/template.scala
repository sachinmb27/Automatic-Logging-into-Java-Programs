import java.io.{File, PrintWriter}
import java.util
import scala.util.control._


object template extends App {

  val loop = new Breaks
  class ArgumentRecordings {
    var argumentName: String = null
    val argumentvalues = new util.ArrayList[ArgumentValues]

    def getArgumentvalues: util.ArrayList[ArgumentValues] = argumentvalues

    def addArgumentvalues(argumentvalues: ArgumentValues): Unit = {
      this.argumentvalues.add(argumentvalues)
    }

    def getArgumentName: String = argumentName

    def setArgumentName(argumentName: String): Unit = {
      this.argumentName = argumentName
    }
  }

  class ArgumentValues(var lineNumber: Int, var typeofStatement: String, var values: String) {
    def getLineNumber: Int = lineNumber

    def getTypeofStatement: String = typeofStatement

    def getValues: String = values

  }

  var totalTrace = ""
  var arg: util.ArrayList[ArgumentRecordings] = null

  def instrum(lineNumber: Int, typeOfStatement: String, args: String*): Unit = {
    totalTrace += "\nLine Number: " + lineNumber + " Type of statement: " + typeOfStatement + " Parameters: "
    val arg_len: Int = args.length
    var i: Int = 0
    while (i < arg_len) {
      totalTrace += args(i) + ":"
      totalTrace += args(i + 1) + ", "
      //traceTable(lineNumber, typeOfStatement, args(i), args(i + 1))
      i += 2
    }
  }

  def printTrace(): Unit = {
    val trace = new File("trace.txt")
    new PrintWriter(trace) { write(totalTrace); close }
    printelementTable
  }

  def printelementTable = {
    var printelementTrace: String = ""
    arg.forEach{args: ArgumentRecordings =>
      printelementTrace += "\n-------------------" + args.getArgumentName + "--------------------"
      args.getArgumentvalues.forEach{argvalues: ArgumentValues =>
        printelementTrace += "\nUsed at Line Number: " + argvalues.getLineNumber
        printelementTrace += "\nType of statement: " + argvalues.getTypeofStatement
        printelementTrace += "\nValue: " + argvalues.getValues
      }
    }

    val trace1 = new File("tracetable.txt")
    new PrintWriter(trace1) { write(printelementTrace); close}
  }

  def traceTable(lineNumber: Int, typeOfStatement: String, argument: String, argumentValue: String) = {
    val argumentValues: ArgumentValues = new ArgumentValues(lineNumber, typeOfStatement, argumentValue)
    if(arg == null) {
      val args: ArgumentRecordings = new ArgumentRecordings
      args.setArgumentName(argument)
      args.addArgumentvalues(argumentValues)
      arg = new util.ArrayList[ArgumentRecordings]
      arg.add(args)
    }
    else {
      var counter: Int = 0
      arg.forEach{allarg: ArgumentRecordings =>
        if(allarg.getArgumentName == argument) {
          allarg.addArgumentvalues(argumentValues)
          counter += 1
        }
      }

      if(counter == 0) {
        val args: ArgumentRecordings = new ArgumentRecordings
        args.setArgumentName(argument)
        args.addArgumentvalues(argumentValues)
        arg.add(args)
      }
    }
  }
}
