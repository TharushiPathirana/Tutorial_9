

object Q3 {
  def main(args: Array[String]): Unit = {
    val accountA = new Account("A123", 1, 1000)
    val accountB = new Account("B456", 2, 500)

    accountA.deposit(200)
    println(accountA)

    accountA.transfer(300, accountB)
    println(accountA)
    println(accountB)
  }

  class Account(nic: String, val accId: Int, var balance: Double = 0.0) {
    require(balance >= 0, "Initial balance cannot be negative")

    def deposit(amount: Double): Unit = {
      require(amount > 0, "Deposit amount must be positive")
      balance += amount
    }

    def withdraw(amount: Double): Unit = {
      require(amount > 0, "Withdrawal amount must be positive")
      require(amount <= balance, "Insufficient balance")
      balance -= amount
    }

    def transfer(amount: Double, toAccount: Account): Unit = {
      require(amount > 0, "Transfer amount must be positive")
      require(amount <= balance, "Insufficient balance for transfer")

      withdraw(amount)
      toAccount.deposit(amount)
    }

    override def toString: String = s"[$nic:$accId:$balance]"
  }
}
