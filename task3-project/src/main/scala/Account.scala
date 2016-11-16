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
    transactions.values.exists(t => !t.isCompleted)
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
    transactions += t.id -> t
    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
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
      // Process receipt
      ???
    }

    case BalanceRequest => getBalanceAmount

    case t: Transaction => {
      // Handle incoming transaction
      ???
    }

    case msg => ???
  }


}
