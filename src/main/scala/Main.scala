import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

object Main {
  def main(args: Array[String]): Unit = {
    // Instantiation of characters // name, isMale, hasHair, wearsGlasses, wearsHut

    val character1 = Character("Tom",isMale = true, hasHair = true, wearsGlasses = true, wearsHut = false)
    val character2 = Character("John",isMale = true, hasHair = true, wearsGlasses = false, wearsHut = true)
    val character3 = Character("Anthos",isMale = true, hasHair = false, wearsGlasses = true, wearsHut = true)
    val character4 = Character("Kokos",isMale = true, hasHair = true, wearsGlasses = true, wearsHut = true)
    val character5 = Character("Will",isMale = true, hasHair = false, wearsGlasses = false, wearsHut = false)

    val character6 = Character("April",isMale = false, hasHair = true, wearsGlasses = true, wearsHut = false)
    val character7 = Character("Maria",isMale = false, hasHair = true, wearsGlasses = false, wearsHut = true)
    val character8 = Character("Andrea",isMale = false, hasHair = true, wearsGlasses = false, wearsHut = false)
    val character9 = Character("Connie",isMale = false, hasHair = true, wearsGlasses = true, wearsHut = true)

    // List of characters (board)
    var listOfCharacters:List[Character] = List(character1,
      character2,character3,character4,character5,character6,character7,
      character8,character9)

    //Pick random Character
    // Player 1
    val randomCharacter1:Character = pickRandomCharacter(listOfCharacters)
    println("Player 1: " + randomCharacter1.name)
    // Remove the random character from player1 list
   var listOfCharacters1:List[Character] = listOfCharacters.filter(char=> char!=randomCharacter1)
    println("Board of Player 2 (Characters left to choose):")
    printCollection(listOfCharacters1)
    var numberOfCharactersPlayer1HasToGuessFrom:Int = listOfCharacters1.length
    println("Number of characters Player 1 has to guess from " + numberOfCharactersPlayer1HasToGuessFrom)

    //Player 2
    val randomCharacter2:Character = pickRandomCharacter(listOfCharacters)
        println("Player 2: " + randomCharacter2.name)
    // Remove the random character from player2 list
    var listOfCharacters2:List[Character] = listOfCharacters.filter(char=> char!=randomCharacter2)
    println("Board of Player 1 (Characters left to choose):")
    printCollection(listOfCharacters2)
    var numberOfCharactersPlayer2HasToGuessFrom:Int = listOfCharacters2.length
    println("Number of characters Player 2 has to guess from " + numberOfCharactersPlayer2HasToGuessFrom)

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

    while(numberOfCharactersPlayer2HasToGuessFrom!=1 && numberOfCharactersPlayer1HasToGuessFrom!=1){
//    First Player
      println("Player 1 is playing")
      println("Number of characters player 1 has to guess from " + numberOfCharactersPlayer1HasToGuessFrom)
      println("")
      val randomQuestion1 = pickRandomQuestion(mapOfQuestionsPlayer1)
      var yesOrNo = scala.io.StdIn.readLine(randomQuestion1)
      listOfCharacters1 = removeCharacter(yesOrNo, listOfCharacters1, mapOfQuestionsPlayer1,randomQuestion1)
      numberOfCharactersPlayer1HasToGuessFrom = listOfCharacters1.length
      mapOfQuestionsPlayer1 = removeQuestionFromQuestionsOfPlayer(randomQuestion1, mapOfQuestionsPlayer1)
      printCollection(listOfCharacters1)
      println("")

      if (numberOfCharactersPlayer1HasToGuessFrom==1){
        println("Player 1 Won")
        break
      }

//      Second Player
      println("Player 2 is playing")
      println("Number of characters player 2 has to guess from " + numberOfCharactersPlayer2HasToGuessFrom)
      println("")
      val randomQuestion2 = pickRandomQuestion(mapOfQuestionsPlayer2)
      yesOrNo = scala.io.StdIn.readLine(randomQuestion2)
      listOfCharacters2 = removeCharacter(yesOrNo, listOfCharacters2, mapOfQuestionsPlayer2,randomQuestion2)
      numberOfCharactersPlayer2HasToGuessFrom = listOfCharacters2.length
      mapOfQuestionsPlayer2 = removeQuestionFromQuestionsOfPlayer(randomQuestion2, mapOfQuestionsPlayer2)
      printCollection(listOfCharacters2)
      println("")

      if (numberOfCharactersPlayer2HasToGuessFrom==1){
        println("Player 2 Won")
        break
      }
    }

  }

  // Function that pick a random character from a collection of characters
  def pickRandomCharacter(listOfCharacters:List[Character]): Character ={
    // Generate a random index within the bounds of the collection
    val randomIndex = Random.nextInt(listOfCharacters.length)
    // Access the element at the random index
    val randomCharacter = listOfCharacters(randomIndex)
    randomCharacter
  }

  // Function that picks a random question from a collection of questions
  def pickRandomQuestion(mapOfQuestions:Map[String, Character => Boolean]): String = {
    // Convert map to seq
    val seqOfQuestions:Seq[String] = mapOfQuestions.keys.toSeq
    // Generate a random index within the bounds of the collection
    var randomIndex = Random.nextInt(seqOfQuestions.length)
    //Access the element at thr random index
    val randomQuestion = seqOfQuestions(randomIndex)
    randomQuestion
  }

  // Function that removes a question from player's collection of questions
  def removeQuestionFromQuestionsOfPlayer(questionPicked:String, mapOfQuestions:Map[String, Character => Boolean]):Map[String, Character => Boolean]={
    val updatedMapOfQuestions = mapOfQuestions.removed(questionPicked)
    updatedMapOfQuestions
  }

  // Functions that removes the characters with the specified criterion
  def removeCharacter(yesOrNo:String, listOfCharacters:List[Character], mapOfQuestions:Map[String, Character => Boolean], randomQuestion:String):List[Character] = {
  val trueOrFalse:Boolean = if (yesOrNo=="yes"){
          true
        }else{
          false
        }

    val criterion:Character => Boolean = mapOfQuestions(randomQuestion)
//      println(mapOfQuestions(randomQuestion))
    val remainingChars:List[Character] = listOfCharacters.filter(char => criterion(char)==trueOrFalse)

    remainingChars
    }

  // Function that prints the collection of characters
  def printCollection(listOfCharacters:List[Character]):Unit={
    listOfCharacters.foreach {
      char=> println(char.name)
    }
  }

}

