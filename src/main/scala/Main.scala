object Main extends App{
  val ll = new LinkedList(
    new Node("Tokyo",
      new Node("Paris",
        new Node("Milan")
      )
    )
  )
  println(ll)
  println(ll.getSize())

  var flightList:LinkedList = new LinkedList()
  println(flightList)
  flightList.addToStart("Rome")
  println(flightList)
  flightList.addToStart("Paris")
  flightList.addToStart("Lisbon")
  flightList.addToStart("Bern")
  flightList.addToStart("Rome")
  println(flightList)
  flightList.addToStart("Tokyo")
  println(flightList)
  flightList.removeFirstElement()
  println(flightList)
  println(flightList.getLastElement().get.item)
  flightList.addToEnd("Sembrancher")
  println(flightList)
  println(flightList.isPresent("Sembrancher"))

  println("Task 6")
  println(flightList)
  println(flightList.findElement("Sembrancher").get.item)
  flightList.swapElements("Paris", "Sembrancher")
  println(flightList)
  flightList.removeLastElement()
  println(flightList)
  flightList.removeElement("Lisbon")
  println(flightList)
  flightList.insertAfter("Sembrancher", "Bamako")
  println(flightList)
}
