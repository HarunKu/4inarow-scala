//save i load
object Main extends App {
  var r=0
  var c=0
  var currentPlayer = 1
  start()
  def start(): Unit = {
    var move=0
    println("----------------------------------------------------------------------------")
    print("Type c if you want to set the board size or press any other key to continue:")
    var size=scala.io.StdIn.readLine()
    if(size.equals("c")){
      while(r==0){
        try{
          println("\nNumber of rows:")
          var a = scala.io.StdIn.readInt()
          println("Number of columns:")
          var b = scala.io.StdIn.readInt()
          if(a>=6 && b>=7 && (b-a).abs<=2 ){
            r=a;
            c=b;
          }else{
            println("\nWrong input! Minimum 6 rows and 7 columns needed. Rows and columns should not differ by more than 2! (ex.(8,11) is not accepted)\n > Please try again \n") 
          }
        }catch{
          case e: NumberFormatException => println("Input needs to be a number! Try again:")
        }
      }
    }else{
      r=6
      c=7
    }
    println("----------------------------------------------------------------------------")
    val board = Array.ofDim[Int](r, c)
    for (i <- 0 until r; j <- 0 until c) board(i)(j) = 0
    var moves1: Array[Int] = Array()
    var moves2: Array[Int] = Array()
    while(!checkWin(board) && move <(r*c)){
      printBoard(board)
      println("Player one moves: "+ moves1.mkString(","))
      println("Player two moves: "+moves2.mkString(","))
      val column = readAndVerifyInt()
      if(play(column, currentPlayer, board)) {
        if(currentPlayer ==1){
          currentPlayer = 2
          moves1 = moves1:+ column
        }else{
          currentPlayer=1
          moves2 = moves2:+ column
        }
        move +=1
      } else {
        println("That column is full. Please choose a different one.")
      }
    }
    printBoard(board)
    if(move == (r*c)){
      println("It is a draw!")
    }else{
      if(currentPlayer ==1) currentPlayer=2 else currentPlayer=1
      println("PLAYER " + currentPlayer + " WINS!")
    }
    println("Do you want to play again? (y/n):")
    var choice = scala.io.StdIn.readLine()
    if(choice.equals("y")){
      var currentPlayer = 1
      r=0
      c=0
      start()
    }
  }
  def printBoard(board: Array[Array[Int]]): Unit = {
    println("--"*c)
    for (i <- 0 until r) {
      for (j <- 0 until c) print(board(i)(j) + " ")
      println()
    }
    println("--"*c)
    for (j <- 0 until c) print(j + " ")
    println()
  }
  
  def play(column: Int, player: Int, board: Array[Array[Int]]): Boolean = {
    for(row <- r-1 to 0 by -1) {
      if(board(row)(column) == 0) {
        board(row)(column) = player
        return true
      }
    }
    false
  }
  def readAndVerifyInt() : Int = {
      try{
        print("Player "+currentPlayer+" enter column to drop a token: ")
        var column = scala.io.StdIn.readInt()
        if (column >= 0 && column < c) {
            return column
        } else {
            println(" > Please try again")
            readAndVerifyInt()
          }
      }catch{
          case e: NumberFormatException => readAndVerifyInt()
      }
  }
  def checkWin(board: Array[Array[Int]]): Boolean = {
    // check rows
    for(row <- 0 to r-1) {
      for(col <- 0 to c-4) {
        if(board(row)(col) != 0 && board(row)(col) == board(row)(col + 1) &&
           board(row)(col) == board(row)(col + 2) && board(row)(col) == board(row)(col + 3)) {
          return true
        }
      }
    }
    // check columns
    for(row <- 0 to r-4) {
      for(col <- 0 to c-1) {
        if(board(row)(col) != 0 && board(row)(col) == board(row + 1)(col) &&
           board(row)(col) == board(row + 2)(col) && board(row)(col) == board(row + 3)(col)) {
          return true
        }
      }
    }
    // check diagonal
    for(row <- 0 to r-4) {
      for(col <- 0 to c-4) {
        if(board(row)(col) != 0 && board(row)(col) == board(row + 1)(col + 1) &&
           board(row)(col) == board(row + 2)(col + 2) && board(row)(col) == board(row + 3)(col + 3)) {
          return true
        }
      }
    }
    for(row <- 0 to r-4) {
      for(col <- 3 to c-1) {
        if(board(row)(col) != 0 && board(row)(col) == board(row + 1)(col - 1) &&
           board(row)(col) == board(row + 2)(col - 2) && board(row)(col) == board(row + 3)(col - 3)) {
          return true
        }
      }
    }
    false
  }
}
