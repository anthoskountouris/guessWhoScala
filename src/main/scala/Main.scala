import scala.annotation.tailrec
import scala.util.Random

object Main extends App {

  // Instantiation of characters // name, isMale, hasHair, wearsGlasses, wearsHut
  // List of characters (board)

  val listOfCharacters: List[Character] = List(
    Character("Tom", isMale = true, hasHair = true, wearsGlasses = true, wearsHut = false),
    Character("John", isMale = true, hasHair = true, wearsGlasses = false, wearsHut = true),
    Character("Anthos", isMale = true, hasHair = false, wearsGlasses = true, wearsHut = true),
    Character("Kokos", isMale = true, hasHair = true, wearsGlasses = true, wearsHut = true),
    Character("Will", isMale = true, hasHair = false, wearsGlasses = false, wearsHut = false),
    Character("April", isMale = false, hasHair = true, wearsGlasses = true, wearsHut = false),
    Character("Maria", isMale = false, hasHair = true, wearsGlasses = false, wearsHut = true),
    Character("Andrea", isMale = false, hasHair = true, wearsGlasses = false, wearsHut = false),
    Character("Connie", isMale = false, hasHair = true, wearsGlasses = true, wearsHut = true)
  )

  // Function that pick a random character from a collection of characters
  def pickRandomCharacter(listOfCharacters: List[Character]): Character = {
    // Generate a random index within the bounds of the collection
    val randomIndex = Random.nextInt(listOfCharacters.length)
    // Access the element at the random index
    val randomCharacter = listOfCharacters(randomIndex)
    randomCharacter
  }

  // Function that prints the collection of characters
  def printCollection(listOfCharacters: List[Character]): Unit = {
    println("Available characters:")
    listOfCharacters.foreach {
      char => println(char.name)
    }
    println("")
  }

  def mainDataGenerationPrintingInformation(listOfCharacters: List[Character], pickRandomCharacter: (List[Character]) => Character, printCollection: (List[Character]) => Unit): (Character, List[Character], Int, Map[String, Character => Boolean]) = {
    //Pick random Character
    // Player 1
    val randomCharacter: Character = pickRandomCharacter(listOfCharacters)
    println("Randomly Selected Character: " + randomCharacter.name)

    // Remove the random character from player1 list
//    var listOfCharacters1: List[Character] = listOfCharacters.filter(char => char != randomCharacter)

    printCollection(listOfCharacters)

    var numberOfCharactersPlayer1HasToGuessFrom: Int = listOfCharacters.length
//    println("Number of characters Player 1 has to guess from " + numberOfCharactersPlayer1HasToGuessFrom)
//    println("")
    // Mapping the questions with their criterion in order to remove the right characters from the lists for both Player 1 and Player 2
//    var mapOfQuestionsPlayer1: Map[String, Character => Boolean] = Map("Is it a male? (yes/no)" -> (_.isMale), "Do they have hair? (yes/no)" -> (_.hasHair), "Do they wear glasses? (yes/no)" -> (_.wearsGlasses), "Do they wear a hut? (yes/no)" -> (_.wearsHut))
    var mapOfQuestionsPlayer1: Map[String, Character => Boolean] = Map("Is it a male? (yes/no)" -> (_.isMale), "Do they have hair? (yes/no)" -> (_.hasHair), "Do they wear glasses? (yes/no)" -> (_.wearsGlasses), "Do they wear a hut? (yes/no)" -> (_.wearsHut))
//
//    println("")
//    println("A character has been randomly picked")
//    println("Player 1 character: " + randomCharacter.name)
//    println("")

    (randomCharacter, listOfCharacters, numberOfCharactersPlayer1HasToGuessFrom, mapOfQuestionsPlayer1)
  }


  // Function that picks a random question from a collection of questions
  def pickRandomQuestion(mapOfQuestions: Map[String, Character => Boolean]): String = {
    // Convert map to seq
    val seqOfQuestions: Seq[String] = mapOfQuestions.keys.toSeq
    // Generate a random index within the bounds of the collection
    var randomIndex = Random.nextInt(seqOfQuestions.length)
    //Access the element at thr random index
    val randomQuestion = seqOfQuestions(randomIndex)
    randomQuestion
  }

  // Function that removes a question from player's collection of questions
  def removeQuestionFromQuestionsOfPlayer(questionPicked: String, mapOfQuestions: Map[String, Character => Boolean]): Map[String, Character => Boolean] = {
    val updatedMapOfQuestions = mapOfQuestions.removed(questionPicked)
    updatedMapOfQuestions
  }

  // Functions that removes the characters with the specified criterion
  def removeCharacter(yesOrNo: String, listOfCharacters: List[Character], mapOfQuestions: Map[String, Character => Boolean], randomQuestion: String): List[Character] = {
    val trueOrFalse: Boolean = yesOrNo == "yes"

    val criterion: Character => Boolean = mapOfQuestions(randomQuestion)
    //      println(mapOfQuestions(randomQuestion))
    val remainingChars: List[Character] = listOfCharacters.filter(char => criterion(char) == trueOrFalse)

    remainingChars
  }

  @tailrec
  def inputOfUser(question:String):String ={
    var yesOrNo = scala.io.StdIn.readLine(question)
    if (yesOrNo=="no" || yesOrNo == "yes"){
      yesOrNo
    }else {
      println("Input a valid answer: yes or no")
      inputOfUser(question)
    }
  }

  var (randomCharacter, listOfCharacters1, numberOfCharactersPlayer1HasToGuessFrom, mapOfQuestionsPlayer1) = mainDataGenerationPrintingInformation(listOfCharacters, pickRandomCharacter, printCollection)


  @tailrec
  def mainLogic(): Unit = {
    var parameter:Boolean = true
    var isCorrectChar:Boolean = false
//    while (parameter) {
    val randomQuestion1 = pickRandomQuestion(mapOfQuestionsPlayer1)
    val yesOrNo = inputOfUser(randomQuestion1)
    listOfCharacters1 = removeCharacter(yesOrNo, listOfCharacters1, mapOfQuestionsPlayer1, randomQuestion1)
    numberOfCharactersPlayer1HasToGuessFrom = listOfCharacters1.length
    mapOfQuestionsPlayer1 = removeQuestionFromQuestionsOfPlayer(randomQuestion1, mapOfQuestionsPlayer1)
    printCollection(listOfCharacters1)

    // If one player is left
    if(listOfCharacters1.length <= 1){
      if (randomCharacter == listOfCharacters1.head) {
        println("You Won")
        println(s"The randomly selected characters was ${randomCharacter.name}")
        parameter = false
        isCorrectChar = true
        println("isCorrectChar: " + isCorrectChar)
      }
      else {
        println("You Lost")
        println(s"The randomly selected characters was ${randomCharacter.name}")
        parameter = false
        println("isCorrectChar: " + isCorrectChar)
      }
      isCorrectChar
    }else{
      parameter = true
    }
    if(parameter) mainLogic()
  }

  mainLogic()

}



