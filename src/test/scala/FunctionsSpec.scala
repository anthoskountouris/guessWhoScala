import org.scalatest.flatspec.AnyFlatSpec
import Main.{pickRandomCharacter, pickRandomQuestion, removeCharacter, removeQuestionFromQuestionsOfPlayer}


class FunctionsSpec extends AnyFlatSpec {
  "pickRandomCharacter" should "pick a random character from a list" in {

    val character1 = Character("Tom",isMale = true, hasHair = true, wearsGlasses = true, wearsHut = false)
    val character2 = Character("John",isMale = true, hasHair = true, wearsGlasses = false, wearsHut = true)
    val character3 = Character("Anthos",isMale = true, hasHair = false, wearsGlasses = true, wearsHut = true)
    val character4 = Character("Kokos",isMale = true, hasHair = true, wearsGlasses = true, wearsHut = true)
    val character5 = Character("Will",isMale = true, hasHair = false, wearsGlasses = false, wearsHut = false)

    val character6 = Character("April",isMale = false, hasHair = true, wearsGlasses = true, wearsHut = false)
    val character7 = Character("Maria",isMale = false, hasHair = true, wearsGlasses = false, wearsHut = true)
    val character8 = Character("Andrea",isMale = false, hasHair = true, wearsGlasses = false, wearsHut = false)
    val character9 = Character("Connie",isMale = false, hasHair = true, wearsGlasses = true, wearsHut = true)

    // Player 1 list of characters (board)
    var listOfCharacters:List[Character] = List(character1,
      character2,character3,character4,character5,character6,character7,
      character8,character9)

//    val randomCharacter = pickRandomCharacter(listOfCharacters)
    assert(listOfCharacters.contains(pickRandomCharacter(listOfCharacters)),"The character is not in the List") // the character e

  }

  "pickRandomQuestion" should "pick a random question from the Map" in {
    val mapOfQuestionsPlayer:Map[String, Character => Boolean] = Map("Is it a male? (yes/no)" -> (_.isMale),"Do they have hair? (yes/no)"-> (_.hasHair),"Do they wear glasses? (yes/no)" -> (_.wearsGlasses),"Do they wear a hut? (yes/no)" -> (_.wearsHut))
    val randomQuestion = pickRandomQuestion(mapOfQuestionsPlayer)

    assert(mapOfQuestionsPlayer.contains(randomQuestion), "The value is not one of the questions")
  }

  "removeQuestionFromQuestionsOfPlayer" should "remove question from the Map of questions" in {
    val mapOfQuestionsPlayer:Map[String, Character => Boolean] = Map("Is it a male? (yes/no)" -> (_.isMale),"Do they have hair? (yes/no)"-> (_.hasHair),"Do they wear glasses? (yes/no)" -> (_.wearsGlasses),"Do they wear a hut? (yes/no)" -> (_.wearsHut))
    val randomQuestion = pickRandomQuestion(mapOfQuestionsPlayer)
    val newMapOfQuestions = removeQuestionFromQuestionsOfPlayer(randomQuestion, mapOfQuestionsPlayer)

    assert (newMapOfQuestions.contains(randomQuestion) === false, "The question is not in the Map collection")
  }

  "removeCharacter" should "removes the characters with the specified criterion" in {
    val character1 = Character("Tom", isMale = true, hasHair = true, wearsGlasses = true, wearsHut = false)
    val character2 = Character("John", isMale = true, hasHair = true, wearsGlasses = false, wearsHut = true)
    val character3 = Character("Anthos", isMale = true, hasHair = false, wearsGlasses = true, wearsHut = true)
    val character4 = Character("Kokos", isMale = true, hasHair = true, wearsGlasses = true, wearsHut = true)
    val character5 = Character("Will", isMale = true, hasHair = false, wearsGlasses = false, wearsHut = false)

    val character6 = Character("April", isMale = false, hasHair = true, wearsGlasses = true, wearsHut = false)
    val character7 = Character("Maria", isMale = false, hasHair = true, wearsGlasses = false, wearsHut = true)
    val character8 = Character("Andrea", isMale = false, hasHair = true, wearsGlasses = false, wearsHut = false)
    val character9 = Character("Connie", isMale = false, hasHair = true, wearsGlasses = true, wearsHut = true)

    // Player 1 list of characters (board)
    var listOfCharacters: List[Character] = List(character1,
      character2, character3, character4, character5, character6, character7,
      character8, character9)

    val mapOfQuestionsPlayer: Map[String, Character => Boolean] = Map("Is it a male? (yes/no)" -> (_.isMale), "Do they have hair? (yes/no)" -> (_.hasHair), "Do they wear glasses? (yes/no)" -> (_.wearsGlasses), "Do they wear a hut? (yes/no)" -> (_.wearsHut))
    val randomQuestion = pickRandomQuestion(mapOfQuestionsPlayer)

    assert(removeCharacter("yes", listOfCharacters, mapOfQuestionsPlayer, "Is it a male? (yes/no)") === List(character1, character2, character3, character4, character5))
    assert(removeCharacter("no", listOfCharacters, mapOfQuestionsPlayer, "Is it a male? (yes/no)") === List(character6, character7, character8, character9))

    assert(removeCharacter("yes", listOfCharacters, mapOfQuestionsPlayer, "Do they have hair? (yes/no)") === List(character1, character2, character4, character6, character7, character8, character9))
    assert(removeCharacter("no", listOfCharacters, mapOfQuestionsPlayer, "Do they have hair? (yes/no)") === List(character3, character5))

    assert(removeCharacter("yes", listOfCharacters, mapOfQuestionsPlayer, "Do they wear glasses? (yes/no)") === List(character1, character3, character4, character6, character9))
    assert(removeCharacter("no", listOfCharacters, mapOfQuestionsPlayer, "Do they wear glasses? (yes/no)") === List(character2, character5, character7, character8))

    assert(removeCharacter("yes", listOfCharacters, mapOfQuestionsPlayer, "Do they wear a hut? (yes/no)") === List(character2, character3, character4, character7, character9))
    assert(removeCharacter("no", listOfCharacters, mapOfQuestionsPlayer, "Do they wear a hut? (yes/no)") === List(character1, character5, character6, character8))

    }

  }
