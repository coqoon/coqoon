package dk.itu.sdg.coq

class CoqTestSpec extends CoqSpec {
  "Parsing a simple class can be checked by Coq" should "be ok" in {
    runCoq("Cell.txt", "Cell") match {
      case None => //internal error
      case Some((warn, errs)) =>
        warn should equal(List())
        errs should equal(List())
    }
  }

}
