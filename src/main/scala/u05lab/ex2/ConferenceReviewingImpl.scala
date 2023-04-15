package u05lab.ex2

import u05lab.ex2.Question.*
import u05lab.ex2.ConferenceReviewing

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews: List[(Int, Map[Question, Int])] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = reviews.appended((article, scores))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    loadReview(article, Map(
      RELEVANCE -> relevance,
      SIGNIFICANCE -> significance,
      CONFIDENCE -> confidence,
      FINAL -> fin
    ))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(r => r._1 == article)
      .map(s => s._2(question))
      .sorted

  override def averageFinalScore(article: Int): Double =
    val scores = reviews.filter(r => r._1 == article)
      .map(s => s._2(FINAL))
    scores.sum / scores.length.toDouble

  override def acceptedArticles(): Set[Int] =
    reviews.filter((a, s) => averageFinalScore(a) > 5 && s(RELEVANCE) >= 8)
      .map((k, _) => k).toSet

  override def sortedAcceptedArticles(): List[(Int, Double)] =
    acceptedArticles().map(e => (e, averageFinalScore(e)))
      .toList
      .sortWith(_._2 < _._2)

  private def averageWeightedFinalScore(article: Int): Double =
    val scores = reviews.filter(r => r._1 == article)
      .map(s => s._2(FINAL) * s._2(CONFIDENCE) / 10.0)
    scores.sum / scores.length.toDouble

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews.map((a, s) => a -> averageWeightedFinalScore(a)).toMap
