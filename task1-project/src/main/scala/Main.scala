object Main extends App {

  def thread(body: => Unit): Thread = {
      val t = new Thread {
        override def run() = body
      }
      t.start
      t
    }

  // Write a few transaction examples using Threads
  var account1 = new Account(2000, Bank.getUniqueId)
  var account2 = new Account(1500, Bank.getUniqueId)

  println("Account1 balance: " + account1.getBalanceAmount)
  println("Account2 balance: " + account2.getBalanceAmount)

  var t1 = thread(Bank.transaction(account1, account2, 500))
  var t2 = thread(Bank.transaction(account2, account1, 1000))

  println("Account1 balance: " + account1.getBalanceAmount)
  println("Account2 balance: " + account2.getBalanceAmount)
  println("Pausing main thread for 2 seconds")
  Thread.sleep(2000)
  println("Account1 balance: " + account1.getBalanceAmount)
  println("Account2 balance: " + account2.getBalanceAmount)

}
