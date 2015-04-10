class Folder {

  //First exercise

  val list =List("kayla", "james", "Laura", "Mark", "Priscilla", "Liam");
  val length = list.foldLeft(0)((sum,value) => sum + value.length);
  
  println("Number of chars: " + length+ "\n");
    
}

trait Censor {

  var curseWordsMap = Map[String,String]();
  var curseWordsList = List[String]();
  
	def initialize();

	def replaceCurseWords(wordsList: List[String]);


}

class Implementer extends Censor{

  val wordsList = List("Happy", "Tree", "Friends", "Darn", "Shoot", "Lover", "something", "Shoot");

  initialize();
  replaceCurseWords(wordsList);
  
  def initialize() = {
    val lines = scala.io.Source.fromFile("words.txt").getLines;
    
    for(line <- lines){
      val values = line.split(",");
      curseWordsList = this.curseWordsList ++ List(values(0));
      curseWordsMap = this.curseWordsMap ++ Map(values(0)-> values(1));
    }

    println("Curse words " + this.curseWordsList);
    println("Replacing words "+ this.curseWordsMap);

  }

  def replaceCurseWords(wordsList: List[String]) = {
    println();
    println("Before " + wordsList);
    println();
      
    val replacedList = wordsList.map(i => if (curseWordsList.contains(i)) curseWordsMap(i) else i);

    
    println("After " + replacedList);
    println();

  }
  
}

val folder = new Folder();
val implementer = new Implementer();