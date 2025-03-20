import java.util.concurrent.atomic.AtomicInteger
trait BankAccount {

  def closeAccount(): Unit
  def getBalance: Option[Int]
  def incrementBalance(increment: Int): Option[Int]
}

class BankAccountImpl extends BankAccount {
  private var balance = new AtomicInteger(0)
  private var open: Boolean = true

  override def closeAccount(): Unit =
    open = false
  override def getBalance: Option[Int] =
    if (open) Some(balance.get()) else None
  override def incrementBalance(increment: Int): Option[Int] =
    Some(balance.addAndGet(increment))
}

object Bank { 
  def openAccount(): BankAccount = new BankAccountImpl
}
