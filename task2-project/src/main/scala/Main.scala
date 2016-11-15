
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
  dnb.addTransactionToQueue(ntnu, espen, 1000)
  dnb.addTransactionToQueue(espen, ntnu, 2000)
  Thread.sleep(1000)
  println(espen.getBalanceAmount)
  println(dnb.getProcessedTransactionsAsList.length)
}