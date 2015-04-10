

class Cell {

  val emptyChar = ".";
  var currentValue = emptyChar;
  val players = List("X", "O");
  
  def getCurrentValue():String = currentValue;
  
  def isTaken():Boolean = {
    return currentValue !=emptyChar;
  }
  
  def canOccupyCell(value:String):Boolean = {
    return players.contains(value);
  }
  
  def setCurrentValue (newValue:String){
    
    if(canOccupyCell(newValue)){
        currentValue = newValue;
    }  
    
  }
}

class Board {

	//This variable determines the size of the board (3x3) (4x4)
    val rowSize = 3; 
	val boardLength = rowSize*rowSize;
	var rows = List[List[Int]]();
	var columns = List[List[Int]]();
	var diagonals = List[List[Int]]();

	var hasWinner = false;
	var currentPlayer = "X";

    fillColumns();
	fillRows();
	fillDiagonals();

	val allPaths:List[List[Int]] = rows ++ columns ++ diagonals;

	var hasCells = false;
	var cells:Array[Cell] = new Array[Cell](boardLength);

  //Board initialization
  
  def fillColumns() { 

    for(i<-0 until rowSize){
      var path =List(i);
      for(j<-1 until rowSize){
        path = path ++ List(i+(rowSize*j));
      }
      columns = columns ++ List(path);
    }
    
  }

  def fillRows() { 

    for(i<-0 until rowSize){
      var path =List(i*rowSize);
      for(j<-1 until rowSize){
        path = path ++ List((i*rowSize)+j);
      }
      rows = rows ++ List(path);
    }
    
  }


  def fillDiagonals() { 
    var firstDiagonal =List[Int]();
    
    for(i<-0 until rowSize){
      firstDiagonal = firstDiagonal ++ List(columns(i)(i));
    }
    diagonals = diagonals ++ List(firstDiagonal);

    var secondDiagonal =List[Int]();
    var j=rowSize-1;

    for(i<-0 until rowSize){
      secondDiagonal = secondDiagonal ++ List(columns(j)(i));
      j = j-1;
    }
    diagonals = diagonals ++ List(secondDiagonal);
    
  }
  
  
	def addCellAtIndex(cell:Cell, index:Int) = {
		cells.update(index, cell);
	}

	def hasValidMoves():Boolean = {
			if(hasCells){
				for(cell <- cells){
					if(cell.isTaken()){
						return true;
					}
				}
			}
			return false;

	}

	def hasAnotherMove():Boolean = {
			if(hasWinner){
				return false;
			}
			for(cell <- cells){
				if(!cell.isTaken()){
					return true;
				}
			}
			return false;
	}

	def checkForWinner():String = { 

			val winner = checkAllPaths(allPaths);
			if(winner!=""){
				hasWinner = true;
			}
			return winner;
	}


	def checkAllPaths(list: List[List[Int]]): String = list match {
	  case x :: xs => {
		  evaluatePath(x) + checkAllPaths(xs)
	  }
	  case _ => "" 
	} 

	

	def evaluatePath(path:List[Int]):String = {
			
			var values = List[String](); 
			for(i<-0 until rowSize){
				val player = cells(path(i));
				values = values ++ List(player.currentValue);
			}
			val allX = compareValues(values, "X");
			val allO = compareValues(values, "O");
			if(allX){
				return "X";
			}
			if(allO){
				return "O";
			}
			return ""; 
	}

	def compareValues(list: List[String], rule:String): Boolean = list match {
	  case x :: xs => {
		  x == rule && compareValues(xs, rule);
	  }
	  case _ => true 
	}
	
  def togglePlayer(){

		if(currentPlayer =="X"){
			currentPlayer ="O"
		}else{
		  currentPlayer ="X"
		}
	}
	override def toString():String = {

			var toPrint ="";

			for(i<-0 until boardLength){
				val cell = cells(i);
				toPrint = toPrint + "| "+ cell.currentValue + " ";
				if((i+1)%rowSize==0 && i>0){
					toPrint = toPrint +"|\n";
				}

			}
			return toPrint;
	}

}

class Game {


    var board :Board = new Board();

  def mapStringToBoard(inputBoard:String) = {
	  println("Checking", inputBoard, inputBoard.length, this.board.boardLength);
	  if(inputBoard.length == board.boardLength){
		  for(i<-0 until inputBoard.length){
			  var cell = new Cell();
			  cell.setCurrentValue(inputBoard.charAt(i) +"");
			  board.addCellAtIndex(cell, i);
			  board.hasCells = true;
		  }
	  }

  }

 def play() {
    this.mapStringToBoard(args(0));   
    if(this.board.hasValidMoves()){
        val winner = this.board.checkForWinner();
        println("winner ", winner);
       println(this.board); 
    }   
     
   }
  
  def playWithConsole():Unit = {
		var board :Board = new Board();

    for(i<-0 until board.boardLength){
	    var cell = new Cell();
	    board.addCellAtIndex(cell, i);

    }
    while (board.hasAnotherMove()){
	    println();
	    println( board );

	    print( "\n" + board.currentPlayer + "'s Turn: " );

	    try {

		    var cellIndex = scala.io.StdIn.readInt();

		    var cell = board.cells(cellIndex);
		    if(cell.isTaken()){
			    println("That spot is taken!")
		    }else{
			    cell.setCurrentValue(board.currentPlayer);
			    board.addCellAtIndex(cell, cellIndex);
			    board.checkForWinner();
			    if(board.hasWinner){
				    println(board.currentPlayer  + " won the game!");
			    }else{
				    if(!board.hasAnotherMove()){
					    println("There is a tie! ");
				    }
				    board.togglePlayer();
			    }

		    }


	    } catch {

		    // The selected index is not valid.
	    case e:Exception =>
	      println( "That index is not valid!" );

	    }

    }

    println(board);
    }
  }

var game = new Game();
game.playWithConsole();
