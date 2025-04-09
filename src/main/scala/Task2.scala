object Task2 extends App{
  val list = List[String]("Alexandre", "Paul", "Yannick", "Robert")

  println(list.mkString(","))

  if (list.contains("Paul")) println("Paul est dans la liste")

  val listNoPaul = list.filterNot(_ == "Paul")

  println(listNoPaul.mkString(","))
  println(list.mkString(","))
}
