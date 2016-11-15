import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId


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


  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }


}
