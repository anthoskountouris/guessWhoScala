import scala.collection.immutable.Map
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

object Main {
  def main(args: Array[String]): Unit = {
    // Instantiation of characters // name, isMale, hasHair, wearsGlasses, wearsHut

    val character1 = new Character("Tom",true, true, true, false)
    val character2 = new Character("John",true, true, false, true)
    val character3 = new Character("Anthos",true, false, true, true)
    val character4 = new Character("Kokos",true, true, true, true)
    val character5 = new Character("Will",true, false, false, false)

    val character6 = new Character("April",false, true, true, false)
    val character7 = new Character("Maria",false, true, false, true)
    val character8 = new Character("Andrea",false, true, false, false)
    val character9 = new Character("Connie",false, true, true, true)

    // Player 1 list of characters (board)
    var listOfCharacters1:List[Character] = List(character1,
      character2,character3,character4,character5,character6,character7,
      character8,character9)

    // Player 2 list of characters (board)
    var listOfCharacters2:List[Character] = List(character1,
      character2,character3,character4,character5,character6,character7,
      character8,character9)

    //Pick random Character
    // Player 1
    val randomCharacter1:Character = pickRandomCharacter(listOfCharacters1)
    println("Player 1: " + randomCharacter1.name)
    // Remove the random character from player1 list
    listOfCharacters1 = listOfCharacters1.filter(char=> char!=randomCharacter1)
    println("Board of Player 1:")
    printCollection(listOfCharacters1)
    var numberOfCharactersPlayer2HasToGuessFrom:Int = listOfCharacters1.length
    println("Number of characters Player 2 has to guess from " + numberOfCharactersPlayer2HasToGuessFrom)

    //Player 2
    val randomCharacter2:Character = pickRandomCharacter(listOfCharacters1)
        println("Player 2: " + randomCharacter2.name)
    // Remove the random character from player2 list
    listOfCharacters2 = listOfCharacters2.filter(char=> char!=randomCharacter2)
    println("Board of Player 2:")
    printCollection(listOfCharacters2)
    var numberOfCharactersPlayer1HasToGuessFrom:Int = listOfCharacters2.length
    println("Number of characters Player 1 has to guess from " + numberOfCharactersPlayer1HasToGuessFrom)

    // Mapping the questions with their criterion in order to remove the right characters from the lists for both Player 1 and Player 2
    var mapOfQuestionsPlayer1:Map[String, Character => Boolean] = Map("Is it a male? (yes/no)" -> (_.isMale),"Do they have hair? (yes/no)"-> (_.hasHair),"Do they wear glasses? (yes/no)" -> (_.wearsGlasses),"Do they wear a hut? (yes/no)" -> (_.wearsHut))
    var mapOfQuestionsPlayer2:Map[String, Character => Boolean] = Map("Is it a male? (yes/no)" -> (_.isMale),"Do they have hair? (yes/no)"-> (_.hasHair),"Do they wear glasses? (yes/no)" -> (_.wearsGlasses),"Do they wear a hut? (yes/no)" -> (_.wearsHut))


    // Beginning of game
    println("Hello there! This is the Guess Who game.")
    println("")
    println("A character has been randomly picked for each player")
    println("Player 1 character: " + randomCharacter1.name)
    println("Player 2 character: " + randomCharacter2.name)
    println("")

    //
//    var bool:Boolean = true
//    breakable {
//      while(bool){
//        var yesOrNo = scala.io.StdIn.readLine("Is it a male? (yes/no)")
//        listOfCharacters1 = removeCharacter(yesOrNo, listOfCharacters1, _.isMale)
////        if (!ifCharStillInList(randomCharacter,listOfCharacters)){
////          println("You Lost.")
////          break
////        }
//        printCollection(listOfCharacters1)
//        bool = guessWho(randomCharacter1)
////        println("bool="+bool)
//        if(bool==false){
//          break
//        }
//        yesOrNo = scala.io.StdIn.readLine("Do they have hair? (yes/no)")
//        listOfCharacters1 = removeCharacter(yesOrNo, listOfCharacters1, _.hasHair)
//        printCollection(listOfCharacters1)
//        bool = guessWho(randomCharacter1)
//        if(bool==false){
//          break
//        }
//        yesOrNo = scala.io.StdIn.readLine("Do they wear glasses? (yes/no)")
//        listOfCharacters1 = removeCharacter(yesOrNo, listOfCharacters1, _.wearsGlasses)
//        printCollection(listOfCharacters1)
//        bool = guessWho(randomCharacter1)
//        if(bool==false){
//          break
//        }
//        yesOrNo = scala.io.StdIn.readLine("Do they wear a hut? (yes/no)")
//        listOfCharacters1 = removeCharacter(yesOrNo, listOfCharacters1, _.wearsHut)
//        printCollection(listOfCharacters1)
//        bool = guessWho(randomCharacter1)
//        if(bool==false) {
//          break
//        }
//      }
//    }

    while(numberOfCharactersPlayer2HasToGuessFrom!=1 && numberOfCharactersPlayer1HasToGuessFrom!=1){
//    First Player
      println("Player 1 is playing")
      println("")
      val randomQuestion1 = pickRandomQuestion(mapOfQuestionsPlayer1)
      var yesOrNo = scala.io.StdIn.readLine(randomQuestion1)
      listOfCharacters1 = removeCharacter(yesOrNo, listOfCharacters1, mapOfQuestionsPlayer1,randomQuestion1)
      numberOfCharactersPlayer1HasToGuessFrom = listOfCharacters2.length
      println(numberOfCharactersPlayer1HasToGuessFrom)
      mapOfQuestionsPlayer1 = removeQuestionFromQuestionsOfPlayer(randomQuestion1, mapOfQuestionsPlayer1)
      printCollection(listOfCharacters1)
      println("")

      if (numberOfCharactersPlayer2HasToGuessFrom==1){
        println("Player 1 Won")
        break
      }

//      Second Player
      println("Player 2 is playing")
      println("")
      val randomQuestion2 = pickRandomQuestion(mapOfQuestionsPlayer2)
      yesOrNo = scala.io.StdIn.readLine(randomQuestion2)
      listOfCharacters2 = removeCharacter(yesOrNo, listOfCharacters2, mapOfQuestionsPlayer2,randomQuestion2)
      numberOfCharactersPlayer2HasToGuessFrom = listOfCharacters1.length
      println(numberOfCharactersPlayer2HasToGuessFrom)
      mapOfQuestionsPlayer2 = removeQuestionFromQuestionsOfPlayer(randomQuestion2, mapOfQuestionsPlayer2)
      printCollection(listOfCharacters2)
      println("")

      if (numberOfCharactersPlayer1HasToGuessFrom==1){
        println("Player 2 Won")
        break
      }
    }

  }

  // Function that pick a random character from a collection of characters
  private def pickRandomCharacter(listOfCharacters:List[Character]): Character ={
    // Generate a random index within the bounds of the collection
    val randomIndex = Random.nextInt(listOfCharacters.length)
    // Access the element at the random index
    val randomCharacter = listOfCharacters(randomIndex)
    randomCharacter
  }

  // Function that picks a random question from a collection of questions
  private def pickRandomQuestion(mapOfQuestions:Map[String, Character => Boolean]): String = {
    // Convert map to seq
    val seqOfQuestions:Seq[String] = mapOfQuestions.keys.toSeq
    // Generate a random index within the bounds of the collection
    var randomIndex = Random.nextInt(seqOfQuestions.length)
    //Access the element at thr random index
    val randomQuestion = seqOfQuestions(randomIndex)
    randomQuestion
  }

  // Function that removes a question from player's collection of questions
  private def removeQuestionFromQuestionsOfPlayer(questionPicked:String, mapOfQuestions:Map[String, Character => Boolean]):Map[String, Character => Boolean]={
    val updatedMapOfQuestions = mapOfQuestions.removed(questionPicked)
    updatedMapOfQuestions
  }

  // Functions that removes the characters with the specified criterion
  private def removeCharacter(yesOrNo:String, listOfCharacters:List[Character], mapOfQuestions:Map[String, Character => Boolean], randomQuestion:String):List[Character] = {
  val trueOrFalse:Boolean = if (yesOrNo=="yes"){
          true
        }else{
          false
        }

//    val remainingChars: List[Character] = listOfCharacters.filter(char => criteria(char)==trueOrFalse)
//    remainingChars
//      // Filter the characters based on the values in mapOfQuestions
//      val remainingChars: List[Character] = listOfCharacters.filter { char =>
//        // Check each question in the map and apply its corresponding function to the character
//        mapOfQuestions.values.forall(questionFunc => questionFunc(char) == trueOrFalse)
//      }

    val criterion:Character => Boolean = mapOfQuestions(randomQuestion)
//      println(mapOfQuestions(randomQuestion))
    val remainingChars:List[Character] = listOfCharacters.filter(char => criterion(char)==trueOrFalse)

    remainingChars
    }

//  private def removeCharacter(yesOrNo: String, listOfCharacters: List[Character], criteria: Character => Boolean, mapOfQuestions: Map[String, Character => Boolean], randomQuestion: String): List[Character] = {
//    val trueOrFalse: Boolean = if (yesOrNo == "yes") true else false
//
//    // Filter the characters based on the values in mapOfQuestions
//    val remainingChars: List[Character] = listOfCharacters.filter { char =>
//      // Check each question in the map and apply its corresponding function to the character
//      mapOfQuestions.values.forall(questionFunc => questionFunc(char) == trueOrFalse)
//    }
//
//    remainingChars
//  }

  // Function that prints the collection of characters
  private def printCollection(listOfCharacters:List[Character]):Unit={
    listOfCharacters.foreach {
      char=> println(char.name)
    }
  }

//  private def guessWho(theCharacter:Character):Boolean ={
//    var yesOrNo = scala.io.StdIn.readLine("Wanna guess who it is (yes/no)?")
//    if (yesOrNo == "yes") {
//      val name = scala.io.StdIn.readLine("Who is it (name)?")
//      if (name==theCharacter.name){
//        println(s"That is right! It's ${theCharacter.name}")
//        false
//    }else {
//        println("Nope! Keep going.")
//        true
//    }
//  }else{
//      true
//    }
//  }
//
//  private def ifCharStillInList(char:Character, collection:List[Character]):Boolean={
//    collection.contains(char)
//  }

}