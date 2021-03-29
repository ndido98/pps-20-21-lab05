package u05lab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u05lab.code._

class ExamsManagerTests {

    val em = ExamsManager()

    @Test
    def testSucceededEvaluationRange(): Unit = {
        assertThrows(classOf[IllegalArgumentException], () => Succeeded(17))
        assertThrows(classOf[IllegalArgumentException], () => Succeeded(32))
        assertDoesNotThrow(() => Succeeded(18))
    }

    def prepareExams(): Unit = {
        em.createNewCall("gennaio")
        em.createNewCall("febbraio")
        em.createNewCall("marzo")
        em.addStudentResult("gennaio", "rossi", Failed()) // rossi -> fallito
        em.addStudentResult("gennaio", "bianchi", Retired()) // bianchi -> ritirato
        em.addStudentResult("gennaio", "verdi", Succeeded(28)) // verdi -> 28
        em.addStudentResult("gennaio", "neri", Succeeded(30, cumLaude = true)) // neri -> 30L
        em.addStudentResult("febbraio", "rossi", Failed())
        em.addStudentResult("febbraio", "bianchi", Succeeded(20))
        em.addStudentResult("febbraio", "verdi", Succeeded(30))
        em.addStudentResult("marzo", "rossi", Succeeded(25))
        em.addStudentResult("marzo", "bianchi", Succeeded(25))
        em.addStudentResult("marzo", "viola", Failed())
    }

    @Test
    def testGetAllStudentsFromCall(): Unit = {
        prepareExams()
        // partecipanti agli appelli di gennaio e marzo
        assertEquals(Set("rossi", "bianchi", "verdi", "neri"), em.getAllStudentsFromCall("gennaio"))
        assertEquals(Set("rossi", "bianchi", "viola"), em.getAllStudentsFromCall("marzo"))
    }

    @Test
    def testGetEvaluationsMapFromCall(): Unit = {
        prepareExams()
        // promossi di gennaio con voto
        assertEquals(2, em.getEvaluationsMapFromCall("gennaio").size)
        assertEquals(28, em.getEvaluationsMapFromCall("gennaio")("verdi"))
        assertEquals(30, em.getEvaluationsMapFromCall("gennaio")("neri"))
        // promossi di febbraio con voto
        assertEquals(2, em.getEvaluationsMapFromCall("febbraio").size)
        assertEquals(20, em.getEvaluationsMapFromCall("febbraio")("bianchi"))
        assertEquals(30, em.getEvaluationsMapFromCall("febbraio")("verdi"))
    }

    @Test
    def testGetResultsMapFromStudent(): Unit = {
        prepareExams()
        // tutti i risultati di rossi
        assertEquals(3, em.getResultsMapFromStudent("rossi").size, 3)
        assertEquals(Failed(), em.getResultsMapFromStudent("rossi")("gennaio"))
        assertEquals(Failed(), em.getResultsMapFromStudent("rossi")("febbraio"))
        assertEquals(Succeeded(25), em.getResultsMapFromStudent("rossi")("marzo"))
        // tutti i risultati di bianchi
        assertEquals(3, em.getResultsMapFromStudent("bianchi").size)
        assertEquals(Retired(), em.getResultsMapFromStudent("bianchi")("gennaio"))
        assertEquals(Succeeded(20), em.getResultsMapFromStudent("bianchi")("febbraio"))
        assertEquals(Succeeded(25), em.getResultsMapFromStudent("bianchi")("marzo"))
        // tutti i risultati di neri
        assertEquals(1, em.getResultsMapFromStudent("neri").size)
        assertEquals(Succeeded(30, cumLaude = true), em.getResultsMapFromStudent("neri")("gennaio"))
    }

    @Test
    def testGetBestResultFromStudent(): Unit = {
        prepareExams()
        // miglior voto acquisito da ogni studente, o vuoto..
        assertEquals(Some(25), em.getBestResultFromStudent("rossi"))
        assertEquals(Some(25), em.getBestResultFromStudent("bianchi"))
        assertEquals(Some(30), em.getBestResultFromStudent("neri"))
        assertEquals(None, em.getBestResultFromStudent("viola"))
    }
}
