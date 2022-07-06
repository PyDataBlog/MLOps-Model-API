package stress

object InsertKVStressTest extends AbstractStressTest {
  override def getQueryPattern(index: Integer): String = {
    "insert key(key" + index + ") into db1 data({\"data\":\"example1\"})"
  }
}
