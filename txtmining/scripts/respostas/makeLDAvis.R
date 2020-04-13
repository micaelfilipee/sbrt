makeLDAvis <- function(topic.model, outDir = tempfile(), openBrowser = TRUE, asGist = FALSE, ...){
  phi <- mallet::mallet.topic.words(topic.model, smoothed = T, normalized = T)
  theta <- mallet::mallet.doc.topics(topic.model, smoothed = T, normalized = T)
  doc.length <- rowSums(mallet::mallet.doc.topics(topic.model, smoothed = T, normalized = T))
  word.freqs <- mallet::mallet.word.freqs(topic.model)
  vocab <- topic.model$getVocabulary()
  json <- list(
    phi = phi, theta = theta, doc.length = doc.length, vocab = vocab,
    term.frequency = droplevels(word.freqs)$term.freq)
  jsonLDA <- LDAvis::createJSON(phi = json$phi, theta = json$theta, doc.length = json$doc.length,
                                vocab = json$vocab, term.frequency = json$term.frequency,reorder.topics = F)
  # if you want to put it on Github, need to havae gistr installed
  # this can be done by:
  # devtools::install_github('rOpenSci/gistr')
   library(gistr)
  # see help("serVis") for more details
  if(asGist==TRUE) library(gistr)
  LDAvis::serVis(jsonLDA, out.dir = outDir, open.browser = openBrowser, as.gist = asGist, ... = ...)
}