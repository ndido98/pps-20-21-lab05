package u05lab.code

sealed trait ExamResult
case class Retired() extends ExamResult
case class Failed() extends ExamResult
case class Succeeded(evaluation: Int, cumLaude: Boolean = false) extends ExamResult {
    require((!cumLaude && evaluation >= 18 && evaluation <= 30) || (cumLaude && evaluation == 30))
}

trait ExamsManager {

    def createNewCall(call: String)

    def addStudentResult(call: String, student: String, result: ExamResult)

    def getAllStudentsFromCall(call: String): Set[String]

    def getEvaluationsMapFromCall(call: String): Map[String, Int]

    def getResultsMapFromStudent(student: String): Map[String, ExamResult]

    def getBestResultFromStudent(student: String): Option[Int]
}

object ExamsManager {
    def apply(): ExamsManager = new ExamsManagerImpl

    private class ExamsManagerImpl() extends ExamsManager {

        private var calls: Map[String, Map[String, ExamResult]] = Map()

        override def createNewCall(call: String): Unit =
            calls += (call -> Map.empty)

        override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
            val evaluationsInCall = calls.getOrElse(call, throw new IllegalArgumentException)
            val updated = evaluationsInCall + (student -> result)
            calls += (call -> updated)
        }

        override def getAllStudentsFromCall(call: String): Set[String] =
            calls.getOrElse(call, throw new IllegalArgumentException).keySet

        override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
            val evaluationsInCall = calls.getOrElse(call, throw new IllegalArgumentException)
            evaluationsInCall.collect { case (student, Succeeded(evaluation, _)) => student -> evaluation }
        }

        override def getResultsMapFromStudent(student: String): Map[String, ExamResult] =
            calls.collect { case (call, results) if results contains student => call -> results(student) }

        override def getBestResultFromStudent(student: String): Option[Int] = {
            getResultsMapFromStudent(student).values.collect {
                case Succeeded(evaluation, _) => evaluation
            }.maxByOption(identity)
        }
    }
}