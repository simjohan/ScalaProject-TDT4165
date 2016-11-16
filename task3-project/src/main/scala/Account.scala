import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    transactions.values.foreach(t => if(!t.isCompleted) return false)
    true
  }

  def withdraw(amount: Double): Unit = this.synchronized  {
    if (amount > balance.amount) throw new NoSufficientFundsException("Too poor!")
    if (amount < 0) throw new IllegalAmountException("Invalid amount (negative).")
    balance.amount -= amount
  }

  def deposit(amount: Double): Unit = this.synchronized {
    if (amount <= 0) throw new IllegalAmountException("Must be positive")
    balance.amount += amount
  }

  def getBalanceAmount: Double = {
    balance.amount
  }

  def sendTransactionToBank(t: Transaction): Unit = {
    BankManager.findBank(bankId) ! t
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)
    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
        case _: NoSuchElementException  =>
          println("YOOOO")
      }
    }
    t
  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    case TransactionRequestReceipt(to, transactionId, transaction) => {
      transactions(transactionId).status = transaction.status
    }

    case BalanceRequest => getBalanceAmount

    case t: Transaction => {
      try {
        deposit(t.amount)
        t.status = TransactionStatus.SUCCESS
        BankManager.findBank(bankId) ! new TransactionRequestReceipt(t.from,t.id,t)
      } catch {
        case _: Exception => {
          t.status = TransactionStatus.FAILED
          BankManager.findBank(bankId) ! new TransactionRequestReceipt(t.from,t.id,t)
        }
      }
    }

    case msg => println(msg)
  }


}
