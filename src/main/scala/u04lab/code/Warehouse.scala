package u04lab.code
import List.*
trait Item {
  def code: Int
  def name: String
  def tags: List[String]
}

object Item:
  private case class ItemImpl(
    override val code: Int,
    override val name: String,
    override val tags: List[String]) extends Item {}

  def apply(code: Int, name: String, tags: String*): Item =
    var tagList: List[String] = empty
    for t <- tags do tagList = append(tagList, cons(t, empty))
    ItemImpl(code, name, tagList)

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse {
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): List[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Option[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
}

object Warehouse {

  private class WarehouseImpl() extends Warehouse:
    private var items: List[Item] = empty
    override def store(item: Item): Unit = items = append(items, cons(item, empty))
    override def contains(itemCode: Int): Boolean = List.contains(map(items)(i => i.code), itemCode)
    override def searchItems(tag: String): List[Item] = List.filter(items)(i => List.contains(i.tags, tag))
    override def retrieve(code: Int): Option[Item] = List.find(items)(i => i.code == code)
    override def remove(item: Item): Unit = items = List.remove(items)(i => i == item)

  def apply(): Warehouse = new WarehouseImpl()
}

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

  println(warehouse.contains(dellXps.code)) // false
  println(warehouse.store(dellXps)) // side effect, add dell xps to the warehouse
  println(warehouse.contains(dellXps.code)) // true
  println(warehouse.store(dellInspiron)) // side effect, add dell inspiron to the warehouse
  println(warehouse.store(xiaomiMoped)) // side effect, add xiaomi moped to the warehouse
  println(warehouse.searchItems("mobility")) // List(xiaomiMoped)
  println(warehouse.searchItems("notebook")) // List(dellXps, dellInspiron)
  println(warehouse.retrieve(11)) // None
  println(warehouse.retrieve(dellXps.code)) // Some(dellXps)
  println(warehouse.remove(dellXps)) // side effect, remove dell xps from the warehouse
  println(warehouse.retrieve(dellXps.code)) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/