
object Q4 {
  def main(args: Array[String]): Unit = {
    val accountA = new Account("ACC1", 1000)
    val accountB = new Account("ACC2", 500)
    accountB.withdraw(1000)
    val accountC = new Account("ACC3", 2000)

    val bank = List(accountA, accountB, accountC)

    println("Accounts with negative balances:")
    BankFunctions.accountsWithNegativeBalances(bank).foreach(println)

    println(s"Total balance of all accounts: ${BankFunctions.calculateTotalBalance(bank)}")

    println("Applying interest to accounts:")
    BankFunctions.applyInterestToAccounts(bank)
    bank.foreach(println)
  }

  class Account(val accountNumber: String, var balance: Double = 0.0) {
    require(balance >= 0, "Initial balance cannot be negative")

    def deposit(amount: Double): Unit = {
      require(amount > 0, "Deposit amount must be positive")
      balance += amount
    }

    def withdraw(amount: Double): Unit = {
      require(amount > 0, "Withdrawal amount must be positive")
      balance -= amount
    }

    def applyInterest(): Unit = {
      if (balance > 0) {
        balance *= 1.05
      } else if (balance < 0) {
        balance *= 1.1
      }
    }

    override def toString: String = s"[$accountNumber:$balance]"
  }

  object BankFunctions {
    def accountsWithNegativeBalances(accounts: List[Account]): List[Account] =
      accounts.filter(_.balance < 0)

    def calculateTotalBalance(accounts: List[Account]): Double =
      accounts.map(_.balance).sum

    def applyInterestToAccounts(accounts: List[Account]): Unit =
      accounts.foreach(_.applyInterest())
  }
}
