import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  private val transactions = mutable.Queue[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = transactions.dequeue

  // Return whether the queue is empty
  def isEmpty: Boolean = transactions.isEmpty

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = transactions.enqueue(t)

  // Return the first element from the queue without removing it
  def peek: Transaction = transactions.front

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = transactions.iterator
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var counter = 0

  override def run: Unit = {

    def doTransaction() = {
      try {
        from withdraw amount
        to deposit amount
        status = TransactionStatus.SUCCESS
        processedTransactions push this
      } catch {
        case iae: IllegalAmountException => {
          status = TransactionStatus.FAILED
          processedTransactions push this
        }
        case nsfe: NoSufficientFundsException => {
          if (counter < allowedAttemps) {
            counter += 1
            transactionsQueue push this
          } else {
            status = TransactionStatus.FAILED
            processedTransactions push this
          }
        }
        case e: Exception => {
          println(e.getMessage)
          status = TransactionStatus.FAILED
          processedTransactions push this
        }
      }
    }

    if (from.uid < to.uid) from synchronized {
      to synchronized {
        doTransaction
      }
    } else to synchronized {
      from synchronized {
        doTransaction
      }
    }



  }
}
