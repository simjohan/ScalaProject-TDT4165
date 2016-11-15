import exceptions.{IllegalAmountException, NoSufficientFundsException}

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {

  private var balance = initialBalance

  def withdraw(amount: Double): Unit = this.synchronized{
    if(amount > balance) {
      throw new NoSufficientFundsException("Insufficient funds.")
    }
    else if (amount < 0) {
      throw new IllegalAmountException("Invalid amount (negative).")
    }
    balance =  balance - amount
  }


  def deposit(amount: Double): Unit = this.synchronized {
    if(amount <= 0) {
      throw new IllegalAmountException("You must deposit a positive amount.")
    }
    balance = balance + amount
  }


  def getBalanceAmount: Double = {
    balance
  }

}
