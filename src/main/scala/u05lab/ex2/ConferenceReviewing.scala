package u05lab.ex2

enum Question:
  case Relevance
  case Significance
  case Confidence
  case Final

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

trait ConferenceReviewing:
  def reviews: ListBuffer[(Int, Map[Question, Int])]

  def loadReview(article: Int)(scores: Map[Question, Int]): Unit

  def loadReview(article: Int, relevance: Int, signficance: Int, confidence: Int, fin: Int): Unit

  def orderedScores(article: Int)(question: Question): List[Int]

  def averageFinalScore(article: Int): Double

  def acceptedArticles(): Set[Int]
  /*

  def sortedAcceptedArticles(): List[(Int, Double)]

  def averageWeightedFinalScoreMap(): Map[Int, Double]*/

object ConferenceReviewing:
  def apply(): ConferenceReviewing =
    ConferenceReviewingImpl()

private class ConferenceReviewingImpl extends ConferenceReviewing:
  var reviews: ListBuffer[(Int, Map[Question, Int])] = ListBuffer.empty

   override def loadReview(article: Int)(scores: Map[Question, Int]): Unit =
    if scores.size < Question.values.length then
      throw IllegalArgumentException()
    reviews.addOne((article, scores))

  override def loadReview(article: Int, relevance: Int, signficance: Int, confidence: Int, fin: Int): Unit =
    var scores: Map[Question, Int] = Map.empty
    scores.addOne(Question.Relevance, relevance)
    scores.addOne(Question.Significance, signficance)
    scores.addOne(Question.Confidence, confidence)
    scores.addOne(Question.Final, fin)
    reviews.addOne((article, scores))

  override def orderedScores(article: Int)(question: Question): List[Int] =
    reviews.filter(el => el._1 == article).map(el => el._2(question)).sorted.toList

  override def averageFinalScore(article: Int): Double =
    var articleElements = reviews.filter(el => el._1 == article)map(el => el._2(Question.Final).toDouble)
    articleElements.sum / articleElements.size

  def accepted(article: Int): Boolean =
    averageFinalScore(article) >= 5.0 && reviews.filter(el => el._1 == article).flatMap(el => el._2)
      .exists(el => el._1 == Question.Relevance && el._2 >= 8)


  override def acceptedArticles(): Set[Int] =
    var els = reviews.map(el => el._1).distinct.filter(el => this.accepted(el))
    els.toSet






