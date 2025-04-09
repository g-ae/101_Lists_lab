import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

/**
 * JUnit 5 regression tests for LinkedList laboratory
 *
 * @author Marc Pignat
 * @author P.-A. Mudry
 */
@DisplayName("In this lab, a linked list that you have implemented") class LinkedListTest {
  @Nested
  @DisplayName("Has some basic operations that must work - Task 4") class Task4_Basic_Operations {
    @DisplayName("The constructor must create an instance correctly")
    @Test def test_constructor(): Unit = {
      val n = new LinkedList()
      assertEquals(Option.empty, n.head, "head should be None after calling the constructor")
    }

    @DisplayName("The size of the list should be reported correctly")
    @Test def test_getSize(): Unit = {
      val n = new LinkedList()
      assertEquals(0, n.getSize(), "list is empty, getSize() MUST return 0")
      n.addToStart("a")
      assertEquals(1, n.getSize(), "list of 1 getSize() MUST return 1")
      n.addToStart("b")
      assertEquals(2, n.getSize(), "list of 2 getSize() MUST return 2")
      n.removeElement("b")
      assertEquals(1, n.getSize(), "list of 1 getSize() MUST return 1")
    }

    @DisplayName("The toString method should be implemented")
    @Test def test_toStringImplemented(): Unit = {
      val n = new LinkedList()
      val defaultToString = n.getClass.getName + "@" + Integer.toHexString(System.identityHashCode(n))
      assertNotEquals(n.toString(), defaultToString, "toString() MUST be implemented")
    }

    @DisplayName("The toString method should report the list elements correctly")
    @Test def test_toString(): Unit = {
      val n = new LinkedList()
      assertTrue(n.toString().contains("0"), "toString is expected to show the list size (0)")
      n.addToStart("Roma")
      assertTrue(n.toString().contains("Roma"), "toString is expected to show the element (here 'Roma')")
      assertTrue(n.toString().contains("1"), "toString is expected to show the list size (1)")
      n.addToStart("Geneva")
      assertTrue(n.toString().contains("Roma"), "toString is expected to show the element (here 'Roma')")
      assertTrue(n.toString().contains("Geneva"), "toString is expected to show the element (here 'Geneva')")
      assertTrue(n.toString().indexOf("Geneva") < n.toString().indexOf("Roma"), "toString is expected to show 'Geneva' before 'Roma'")
      assertTrue(n.toString().contains("2"), "toString is expected to show the list size (2)")
    }
  }

  @Nested
  @DisplayName("Has a correct implementation of insertion and removal operators - Task 5") class Task5_Operations {
    @DisplayName("Remove first element works")
    @Test def testRemoveFirstElement(): Unit = {
      val n = new LinkedList()
      n.addToStart("Cathy")
      n.addToStart("Bob")
      n.addToStart("Alice")
      n.removeFirstElement()
      assertEquals(2, n.getSize(), "size MUST be 2 after removing an element from a 3 element list")
      assertEquals("Bob", n.head.get.item, "first element must be 'Bob'")
      n.removeFirstElement()
      assertEquals(1, n.getSize(), "size MUST be 1 after removing an element from a 2 element list")
      assertEquals("Cathy", n.head.get.item, "first element must be 'Cathy'")
      n.removeFirstElement()
      assertEquals(0, n.getSize(), "size MUST be 0 after removing an element from a 1 element list")
      assertEquals(Option.empty, n.head, "Head must be None")
      n.removeFirstElement()
      assertEquals(0, n.getSize(), "size MUST be 0 after removing an element from a 0 element list")
      assertEquals(Option.empty, n.head, "Head must be None")
    }

    @DisplayName("Getting the last element works")
    @Test def testgetLastElement(): Unit = {
      val n = new LinkedList()
      assertEquals(Option.empty, n.getLastElement(), "last element must be None")
      n.addToStart("Cathy")
      n.addToStart("Bob")
      n.addToStart("Alice")
      assertEquals("Cathy", n.getLastElement().get.item, "last element must be 'Cathy'")
      assertEquals(3, n.getSize(), "size MUST be 3 (getLastElement() MUST not remove the element)")
    }

    @DisplayName("Adding an element at the end works")
    @Test def testAddToEnd(): Unit = {
      val n = new LinkedList()
      n.addToEnd("Alice")
      assertEquals("Alice", n.getLastElement().get.item, "last element must be 'Alice'")
      n.addToEnd("Bob")
      assertEquals("Bob", n.getLastElement().get.item, "last element must be 'Bob'")
      n.addToEnd("Cathy")
      assertEquals("Cathy", n.getLastElement().get.item, "last element must be 'Cathy'")
    }

    @DisplayName("Must be able to find elements correctly")
    @Test def testIsPresent(): Unit = {
      val n = new LinkedList()
      assertFalse(n.isPresent("Alice"), "'Alice' MUST not be in the empty list")
      assertFalse(n.isPresent("Bob"), "'Bob' MUST not be in the empty list")
      assertFalse(n.isPresent("Cathy"), "'Cathy' MUST not be in the empty list")
      n.addToEnd("Bob")
      assertFalse(n.isPresent("Alice"), "'Alice' MUST not be in this list")
      assertTrue(n.isPresent("Bob"), "'Bob' MUST be in this list")
      assertFalse(n.isPresent("Cathy"), "'Cathy' MUST not be in this list")
      n.addToEnd("Cathy")
      assertFalse(n.isPresent("Alice"), "'Alice' MUST not be in this list")
      assertTrue(n.isPresent("Bob"), "'Bob' MUST be in this list")
      assertTrue(n.isPresent("Cathy"), "'Cathy' MUST be in this list")
      n.addToStart("Alice")
      assertTrue(n.isPresent("Alice"), "'Alice' MUST be in this list")
      assertTrue(n.isPresent("Bob"), "'Bob' MUST be in this list")
      assertTrue(n.isPresent("Cathy"), "'Cathy' MUST be in this list")
      n.removeFirstElement()
      assertFalse(n.isPresent("Alice"), "'Alice' MUST not be in this list")
      assertTrue(n.isPresent("Bob"), "'Bob' MUST be in this list")
      assertTrue(n.isPresent("Cathy"), "'Cathy' MUST be in this list")
      n.removeFirstElement()
      assertFalse(n.isPresent("Alice"), "'Alice' MUST not be in this list")
      assertFalse(n.isPresent("Bob"), "'Bob' MUST not be in this list")
      assertTrue(n.isPresent("Cathy"), "'Cathy' MUST be in this list")
      n.removeFirstElement()
      assertFalse(n.isPresent("Alice"), "'Alice' MUST not be in the empty list")
      assertFalse(n.isPresent("Bob"), "'Bob' MUST not be in the empty list")
      assertFalse(n.isPresent("Cathy"), "'Cathy' MUST not be in the empty list")
    }
  }

  @Nested
  @DisplayName("Has a correct implementation complex operators - Task 6") class Task6_Operations {
    @Test
    @DisplayName("Can find elements") def testFindElement(): Unit = {
      val n = new LinkedList()
      assertEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST not be in empty list")
      n.addToEnd("Alice")
      assertEquals(n.head, n.findElement("Alice"), "'Alice' MUST be the head element")
      assertEquals(n.getLastElement(), n.findElement("Alice"), "'Alice' MUST be the end element")
      n.addToEnd("Bob")
      assertEquals(n.getLastElement(), n.findElement("Bob"), "'Bob' MUST be the end element")
      assertNotEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST be in the list")
      n.addToEnd("Cathy")
      assertNotEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST be in the list")
      assertNotEquals(Option.empty, n.findElement("Bob"), "'Bob' MUST be in the list")
      assertEquals(n.getLastElement(), n.findElement("Cathy"), "'Cathy' MUST be the end element")
    }

    @Test
    @DisplayName("Can swap elements") def testSwapElements(): Unit = {
      var n = new LinkedList()
      n.addToEnd("Cathy")
      n.addToEnd("Bob")
      n.addToEnd("Alice")
      n.swapElements("Alice", "Bob")
      assertEquals("Bob", n.getLastElement().get.item, "'Bob' MUST be the last element")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Cathy", "Alice")
      assertEquals(n.head, n.findElement("Alice"), "'Alice' MUST be the head element")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Cathy", "Bob")
      assertEquals("Cathy", n.getLastElement().get.item, "'Cathy' MUST be the last element")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Alice", "Dan")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Dan", "Dan")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Bob", "Dan")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Dan", "Bob")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Alice", null)
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Bob", null)
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Cathy", null)
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements("Dan", null)
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements(null, "Alice")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements(null, "Bob")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements(null, "Cathy")
      assertEquals(3, n.getSize(), "size MUST not have changed after swap")
      n.swapElements(null, "Dan")
      n = new LinkedList()
      n.swapElements("Alice", "Bob")
      assertEquals(0, n.getSize(), "size MUST not have changed after swap")
    }

    @Test def testRemoveLastElement(): Unit = {
      var n = new LinkedList()
      n.addToEnd("Alice")
      n.addToEnd("Bob")
      n.addToEnd("Cathy")
      n.removeLastElement()
      assertEquals(2, n.getSize(), "size MUST be 2")
      assertEquals(Option.empty, n.findElement("Cathy"), "'Cathy' MUST not be in this list")
      assertNotEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST be in this list")
      assertNotEquals(Option.empty, n.findElement("Bob"), "'Bob' MUST be in this list")
      n.removeLastElement()
      assertEquals(1, n.getSize(), "size MUST be 1")
      assertEquals(Option.empty, n.findElement("Bob"), "'Bob' MUST not be in this list")
      assertNotEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST be in this list")
      n.removeLastElement()
      assertEquals(0, n.getSize(), "size MUST be 0")
      assertEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST not be in this list")
      n.removeLastElement()
      assertEquals(0, n.getSize(), "size MUST be 0")
      n = new LinkedList()
      n.removeLastElement()
      assertEquals(0, n.getSize(), "size MUST be 0")
    }

    @Test
    @DisplayName("Can remove elements") def testRemoveElement(): Unit = {
      var n = new LinkedList()
      n.addToEnd("Alice")
      n.addToEnd("Bob")
      n.addToEnd("Cathy")
      n.removeElement("Alice")
      assertEquals(2, n.getSize(), "size MUST be 2")
      assertEquals(n.head, n.findElement("Bob"), "'Bob' MUST be head")
      assertEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST not be in this list")
      assertEquals("Cathy", n.getLastElement().get.item, "'Cathy' MUST be at the end")
      n.addToStart("Alice")
      n.removeElement("Bob")
      assertEquals(2, n.getSize(), "size MUST be 2")
      assertEquals(Option.empty, n.findElement("Bob"), "'Bob' MUST not be in this list")
      assertEquals("Cathy", n.getLastElement().get.item, "'Cathy' MUST be at the end")
      n = new LinkedList()
      n.addToEnd("Alice")
      n.addToEnd("Bob")
      n.addToEnd("Cathy")
      n.removeElement("Cathy")
      assertEquals(2, n.getSize(), "size MUST be 2")
      assertEquals(Option.empty, n.findElement("Cathy"), "'Bob' MUST not be in this list")
      assertEquals("Bob", n.getLastElement().get.item, "'Bob' MUST be at the end")
      n.removeElement("Dan")
      n = new LinkedList()
      n.removeElement("Dan")
      n.addToEnd("Alice")
      n.removeElement("Alice")
      assertEquals(0, n.getSize(), "size MUST be 0")
      assertEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST not be in this list")
      n.removeElement("Alice")
      assertEquals(0, n.getSize(), "size MUST be 0")
      assertEquals(Option.empty, n.findElement("Alice"), "'Alice' MUST not be in this list")
    }

    @Test
    @DisplayName("Can insert an element after another") def testInsertAfter(): Unit = {
      val n = new LinkedList()
      n.addToEnd("Alice")
      n.insertAfter("Alice", "Cathy")
      assertEquals(2, n.getSize(), "size MUST be 2")
      assertEquals(n.head, n.findElement("Alice"), "'Alice' MUST be head")
      assertEquals("Cathy", n.getLastElement().get.item, "'Cathy' MUST be at the end")
      n.insertAfter("Alice", "Bob")
      assertEquals(3, n.getSize(), "size MUST be 3")
      assertEquals("Cathy", n.getLastElement().get.item, "'Cathy' MUST be at the end")
      n.insertAfter("Cathy", "Dan")
      assertEquals(4, n.getSize(), "size MUST be 4")
      assertEquals("Dan", n.getLastElement().get.item, "'Dan' MUST be at the end")
      n.insertAfter("Erin", "Frank")
      assertEquals(4, n.getSize(), "size MUST be 4")
      assertEquals("Dan", n.getLastElement().get.item, "'Dan' MUST be at the end")
      assertFalse(n.isPresent("Erin"), "'Erin' MUST not be in the list")
      assertFalse(n.isPresent("Frank"), "'Frank' MUST not be in the list")
      n.insertAfter("Frank", null)
      assertEquals(4, n.getSize(), "size MUST be 4")
      assertEquals("Dan", n.getLastElement().get.item, "'Dan' MUST be at the end")
      assertFalse(n.isPresent("Frank"), "'Frank' MUST not be in the list")
      n.insertAfter(null, "Frank")
      assertEquals(4, n.getSize(), "size MUST be 4")
      assertEquals("Dan", n.getLastElement().get.item, "'Dan' MUST be at the end")
      assertFalse(n.isPresent("Frank"), "'Frank' MUST not be in the list")
    }
  }
}
