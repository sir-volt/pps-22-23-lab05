package u05lab.ex2

import org.junit.Test
import org.junit.Assert.*

class ConferenceReviewingTest:

/**
 * Si consulti la documentazione dell'interfaccia ConferenceReviewing, che modella i risultati del processo di revisione
 * degli articoli di una conferenza. Ogni articolo viene da revisionato da uno o piÃ¹ revisori anonimi, ognuno dei quali fornisce
 * una valutazione (score) da 0 a 10 per 4 diverse "domande", modellate da ConferenceReviewing.Question. Un articolo viene
 * accettato se il valore medio della valutazione alla domanda "FINAL" Ã¨ >5 e se ha almeno una valutazione "RELEVANCE" >= 8.
 *
 * Implementare ConferenceReviewing attraverso una classe ConferenceReviewingImpl con costruttore senza argomenti,
 * in modo che passi tutti i test di cui sotto, realizzati per essere autoesplicativi.
 *
 * Sono considerati opzionali ai fini della possibilitÃ  di correggere l'esercizio, ma concorrono comunque al raggiungimento
 * della totalitÃ  del punteggio:
 * - implementazione dei test opzionali (relativi alla realizzazione del metodo averageWeightedFinalScoreMap)
 * - la qualitÃ  della soluzione, in particolare con minimizzazione di ripetizioni e codice non inutilmente complesso
 *
 * Indicazioni di punteggio:
 * - correttezza della parte obbligatoria: 9 punti
 * - correttezza della parte opzionale: 4 punti
 * - qualitÃ  della soluzione: 4 punti
 *
 * Si tolga il commento al codice del test.
 */
    import u05lab.ex2.Question
    import u05lab.ex2.ConferenceReviewing
    import scala.collection.mutable.Map

    val cr: ConferenceReviewing = ConferenceReviewing()
    // carico una revisione per l'articolo 1:
    cr.loadReview(1, 8, 8, 6, 8) // 4.8 Ã¨ il voto finale pesato (usato da averageWeightedFinalScoreMap)
    // e simile per gli altri
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6
    val map: Map[Question, Int] = Map.empty
    map.put(Question.Relevance, 8)
    map.put(Question.Significance, 8)
    map.put(Question.Confidence, 7)
    map.put(Question.Final, 8)
    cr.loadReview(4)(map);
    cr.loadReview(5, 6, 6, 6, 10); // 6.0
    cr.loadReview(5, 7, 7, 7, 10); // 7.0

    @Test
    def testOrderedScores(): Unit =
      assertEquals(cr.orderedScores(2)(Question.Relevance), List(4,9));
      assertEquals(cr.orderedScores(4)(Question.Confidence), List(6,7,8));
      assertEquals(cr.orderedScores(5)(Question.Final), List(10,10));

    @Test
    def testAverageFinalScore(): Unit =
      assertEquals(cr.averageFinalScore(1),8.5,0.01);
      assertEquals(cr.averageFinalScore(2),7.5,0.01);
      assertEquals(cr.averageFinalScore(3),3.5,0.01);
      assertEquals(cr.averageFinalScore(4),7.0,0.01);
      assertEquals(cr.averageFinalScore(5),10.0,0.01);

    @Test
    def testAcceptedArticles(): Unit =
      assertEquals(cr.acceptedArticles(),Set(1,2,4));

    @Test
    def testSortedAcceptedArticles(): Unit =
      assertEquals(cr.sortedAcceptedArticles(), List((4,7.0), (2,7.5), (1,8.5)));

    @Test
    def optionalTestAverageWeightedFinalScore(): Unit =
      assertEquals(cr.averageWeightedFinalScoreMap()(1), 5.1, 0.01);
      assertEquals(cr.averageWeightedFinalScoreMap()(2),7.5,0.01);
      assertEquals(cr.averageWeightedFinalScoreMap()(3),1.25,0.01);
      assertEquals(cr.averageWeightedFinalScoreMap()(4),4.93,0.01);
      assertEquals(cr.averageWeightedFinalScoreMap()(5),6.5,0.01);
      assertEquals(cr.averageWeightedFinalScoreMap().size,5);
