object Q3 {
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

  def main(args: Array[String]): Unit = {
    var accountList: List[Account] = List()

    def accCreate(nic: String, accId: Int): Unit = {
      val acc = new Account(nic, accId)
      accountList = accountList ::: acc :: Nil
      println(accountList)
    }

    /* Driver Code */

    accCreate("1", 1)
    accCreate("2", 2)

    // Deposit money
    accountList.headOption.foreach(_.deposit(1000))
    println(accountList.headOption.getOrElse("Account not found"))

    // Transfer money
    accountList.headOption.foreach(_.transfer(2, 100.0, accountList))
    println(accountList.tail.headOption.getOrElse("Account not found"))
  }
}
