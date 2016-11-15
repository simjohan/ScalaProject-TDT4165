
object Main extends App {

  def thread(body: =>Unit): Thread = {
    val t = new Thread {
      override def run() = body
    }
    t.start
    t
  }

  val dnb = new Bank()
  val espen = dnb.addAccount(0)
  val ntnu = dnb.addAccount(1000000)
  for (i <- 1 until 50) {
    dnb.addTransactionToQueue(ntnu, espen, i*100)

  }
  Thread.sleep(1000)
  println(espen.getBalanceAmount)
}