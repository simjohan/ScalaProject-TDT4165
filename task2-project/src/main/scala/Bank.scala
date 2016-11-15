import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.forkjoin.ForkJoinPool

object BankIDGenerator {
  private val id = new AtomicInteger()
  def generate() = {
    id.incrementAndGet()
  }
}

class Bank(val allowedAttempts: Integer = 3) {

  private val uid = BankIDGenerator.generate()
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = ???

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
  }

  def generateAccountId: Int = ???

  private def processTransactions: Unit = ???

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

}
