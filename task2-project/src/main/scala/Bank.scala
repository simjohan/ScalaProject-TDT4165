import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext
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
  private val executorContext = ExecutionContext.global


  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    processTransactions
  }

  def generateAccountId: Int = BankIDGenerator.generate()

  private def processTransactions: Unit = {
    if (!(transactionsQueue isEmpty)){
      executorContext execute transactionsQueue.pop
    }
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

}
