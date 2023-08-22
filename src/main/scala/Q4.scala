object Q4 {
  def main(args: Array[String]): Unit = {
    var accountList: List[Account] = List()

    def accCreate(nic: String, accId: Int): Unit = {
      val acc = new Account(nic, accId)
      accountList = accountList ::: acc :: Nil
      println(accountList)
    }

    val find = (a: Int, b: List[Account]) => b.filter(account => account.accId.equals(a))
    val overdraft = (b: List[Account]) => b.filter(account => account.balance < 0.0)
    val totalBalance = (b: List[Account]) => b.foldLeft(0.0)((x, y) => x + y.balance)
    val interest = (b: List[Account]) => b.map(account => if (account.balance > 0) account.balance * 0.05 else account.balance * 0.1)

    /* Driver Code */

    // Create accounts
    accCreate("1", 1)
    accCreate("2", 2)

    // Deposit money
    find(1, accountList)(0).deposit(1000)
    println(find(1, accountList)(0))

    // Transfer money
    find(1, accountList)(0).transfer(2, 100.0, accountList)
    println(find(2, accountList)(0))

    // List of negative balances
    println(overdraft(accountList))

    // Sum of all account balances
    println(totalBalance(accountList))

    // Final balances of all accounts after applying interest
    println(interest(accountList))
  }
}

class Account(nic: String, val accId: Int, var balance: Double = 0.0) {

  def withdraw(amount: Double): Unit = {
    this.balance = this.balance - amount
  }

  def deposit(amount: Double): Unit = {
    this.balance = this.balance + amount
  }

  def transfer(account: Int, amount: Double, accountList: List[Account]): Unit = {
    val transferAccOpt = accountList.find(acc => acc.accId == account)
    transferAccOpt match {
      case Some(transferAcc) =>
        if (balance < amount) {
          println("Insufficient balance")
        } else {
          this.withdraw(amount)
          transferAcc.deposit(amount)
        }
      case None =>
        println("Account not found for transfer")
    }
  }

  override def toString: String = s"[$nic:$accId:$balance]"
}
