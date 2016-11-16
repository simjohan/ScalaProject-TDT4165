import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    BankManager.createAccount(accountCounter.incrementAndGet().toString, bankId, initialBalance)
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    // Use BankManager to look up an account with ID accountId
    Some(BankManager.findAccount(bankId, accountId))
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    Some(BankManager.findBank(bankId))
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance)
    case GetAccountRequest(id) => findAccount(id)
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)


    case t: TransactionRequestReceipt => {
      var toBankId: String = ""
      var isInternal: Boolean = false
      var toAccountId: String = ""

      if(t.toAccountNumber.length == 4){
        isInternal = true
        toBankId = bankId
        toAccountId = t.toAccountNumber
      }else{
        isInternal = t.toAccountNumber.substring(0,4) == bankId
        toBankId = t.toAccountNumber.substring(0,4)
        toAccountId = t.toAccountNumber.substring(4)
      }

      if (isInternal) {
        BankManager.findAccount(bankId, toAccountId) ! t
      } else {
        BankManager.findBank(toBankId) ! t
      }
    }

    case msg => println(msg)
  }

  def processTransaction(t: Transaction): Unit = {
    implicit val timeout = new Timeout(5 seconds)
    var toBankId: String = ""
    var isInternal: Boolean = false
    var toAccountId: String = ""

    if(t.to.length == 4){
      isInternal = true
      toBankId = bankId
      toAccountId = t.to
    }else{
      isInternal = t.to.substring(0,4) == bankId
      toBankId = t.to.substring(0,4)
      toAccountId = t.to.substring(4)
    }
    val transactionStatus = t.status
    

    if (isInternal) {
      println(s"Bank $bankId received an internal transaction to $toAccountId")
      BankManager.findAccount(bankId, toAccountId) ! t
    } else {
      println(s"Bank $bankId received external trans. forwarding to $toBankId (${t.to})")
      try {
        BankManager.findBank(toBankId) ! t
      } catch {
        case e:NoSuchElementException => throw e
      }
    }

  }
}