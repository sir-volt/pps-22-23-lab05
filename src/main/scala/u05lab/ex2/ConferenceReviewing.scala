package u05lab.ex2

enum Question:
  case Relevance
  case Significance
  case Confidence
  case Final

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
/*
trait ConferenceReviewing:
  var reviews: ListBuffer[(Int, Map[Question, Int])]

  def loadReview(article: Int)(scores: Map[Question, Int]): Unit

  def loadReview(article: Int, relevance: Int, signficance: Int, confidence: Int, fin: Int): Unit

  def orderedScores(article: Int)(question: Question): List[Int]

  def averageFinalScore(article: Int): Double

  def acceptedArticles(): Set[Int]

  def sortedAcceptedArticles(): List[(Int, Double)]

  def averageWeightedFinalScoreMap(): Map[Int, Double]*/

class ConferenceReviewing:
  var reviews: ListBuffer[(Int, Map[Question, Int])] = ListBuffer.empty

   def loadReview(article: Int)(scores: Map[Question, Int]): Unit =
    if scores.size < Question.values.length then
      throw IllegalArgumentException()
    reviews.addOne((article, scores))

  def loadReview(article: Int, relevance: Int, signficance: Int, confidence: Int, fin: Int): Unit =
    var scores: Map[Question, Int] = Map.empty
    scores.addOne(Question.Relevance, relevance)
    scores.addOne(Question.Significance, signficance)
    scores.addOne(Question.Confidence, confidence)
    scores.addOne(Question.Final, fin)
    reviews.addOne((article, scores))

  def orderedScores(article: Int)(question: Question): List[Int] =
    reviews.filter(el => el._1 == article).map(el => el._2(question)).sorted.toList

  def averageFinalScore(article: Int): Double =
    var articleElements = reviews.filter(el => el._1 == article)map(el => el._2(Question.Final))
    articleElements.sum / articleElements.size





