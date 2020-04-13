setwd("/home/micael/R_envs/text-mining/arquivo/codigos_overleeaf")
source("get_txt.R")
library(litMagModelling)
source("makeLDAvis.R")
options(java.parameters="-Xmx6g")
library(topicmodels)
library(dfrtopics)

library(devtools)
#install_github("mlinegar/litMagModelling")
library(tidytext)
library(tidyverse)
library(stringr)
library(mallet)
txtdf<-get_txt()
meta<-select(txtdf, -text)
txtdf<-select(txtdf, doc_id, text)
stop_words_sbrt<-"stop_words_sbrt.txt"
sw<-readr::read_lines(stop_words_sbrt)

#############################
tokens <- txtdf %>%
  unnest_tokens(word, text)


# find document-word counts
word_counts <- tokens %>%
  filter(!(nchar(word) == 1))%>% 
  filter(!word %in% sw) %>%
  count(doc_id, word, sort = TRUE) %>%
  ungroup()



coherence<-vector()
n_topics<-vector()
docs <- mallet.import(txtdf$doc_id,txtdf$text ,stop_words_sbrt)
#numero de topicos deve começar a partir de 2 para evitar o erro "Error in products[keep, ]"
topic<-1
for(i in 1:70){
  topic= topic+1
  mallet_model <- MalletLDA(num.topics = topic)
  mallet_model$loadDocuments(docs)
  mallet_model$train(100)
  
  # word-topic pairs
  #terms_sbrt<-tidy(mallet_model)
  
  # document-topic pairs
  #documennts_sbrt<-tidy(mallet_model, matrix = "gamma")
  
  # column needs to be named "term" for "augment.53,0
  
  term_counts <- rename(word_counts, term = word, document = doc_id)
  term_document_sbrt<-augment(mallet_model, term_counts)


  x <- udpipe::document_term_frequencies(term_document_sbrt[, c("document", "term")])
  dtm <- udpipe::document_term_matrix(x)
  
  phi <- mallet::mallet.topic.words(mallet_model, smoothed = T, normalized = T)
  theta <- mallet::mallet.doc.topics(mallet_model, smoothed = TRUE, normalized = TRUE)
  colnames(dtm) <- 1:ncol(dtm)
  colnames(phi) <- 1:ncol(phi)
  colnames(theta) <- 1:ncol(theta)
  
  coherence.model<-textmineR::CalcProbCoherence(phi = phi, dtm = dtm, M = 5)%>%mean()
  n_topics.model<-mallet_model$model$numTopics
  
  coherence[i]<-coherence.model
  n_topics[i]<-n_topics.model

}

dtm<- term_document_sbrt%>%
  cast_dtm(document = document, term= term, value = n)
dtm2<-as.matrix(dtm)
mod = LDA(x=dtm, k=2, method="Gibbs", control=list(alpha=1, delta=0.1, seed=10005))

topicmodels::perplexity(mod, dtm)


theta <- mallet::mallet.doc.topics(mallet_model, smoothed = F, normalized = TRUE)
phi <- mallet::mallet.topic.words(mallet_model, smoothed = T, normalized = T)


wfreq<-mallet::mallet.word.freqs(mallet_model)

voc<-mallet_model$getVocabulary()
mallet_model$model$typeTopicCounts
mallet_model$m


library(text2vec)
dtm2 <- textmineR::CreateDtm(tokens$word, 
                doc_names = tokens$doc_id, 
                ngram_window = c(1, 2))

it = itoken(txtdf$text, progressbar = FALSE, ids = txtdf$doc_id)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5, doc_proportion_min = 0.02)
dtm3 = create_dtm(it, vectorizer = vocab_vectorizer(vocab))
model = LDA$new(2, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr  =
  model$fit_transform(dtm3, n_iter = 100, n_check_convergence = 1,
                      convergence_tol = -1, progressbar = T)
topic_word_distr_10 = model$topic_word_distribution
perplexity(dtm3, topic_word_distr_10, doc_topic_distr)


pm<-as.matrix(phi)

text2vec::perplexity(dtm2,topic_word_distribution=phi,doc_topic_distribution=theta)





set.seed(12345)
x <- udpipe::document_term_frequencies(word_counts[, c("doc_id", "word")])
dtm <- udpipe::document_term_matrix(x)

model <- LDA(dtm, k=2, method = "Gibbs", control = list(iter=100, burnin=200, alpha=5.0))

topicmodels::perplexity(model, dtm, use_theta=F)


phi <- mallet::mallet.topic.words(mallet_model, smoothed = TRUE, normalized = TRUE)
theta <- mallet::mallet.doc.topics(mallet_model, smoothed = TRUE, normalized = TRUE)
doc.length <- rowSums(mallet::mallet.doc.topics(mallet_model, smoothed = FALSE, normalized = FALSE))
word.freqs <- mallet::mallet.word.freqs(mallet_model)
vocab <- mallet_model$getVocabulary()

bet<-mallet_model$model$typeTopicCounts
ll<-mallet_model$model$modelLogLikelihood()
topics_n<-mallet_model$model$numTopics
phi <- mallet::mallet.topic.words(mallet_model)
#n_tokens<-length(word_counts$word)
tok<-mallet_model$model$totalTokens


dtm<- term_document_sbrt%>%
  cast_dtm(document = document, term= term, value = n)

bet<-tidy(mallet_model)%>%
  select(beta)

exp(-sum(log(colSums(exp(phi[,dtm$j])/topics_n)) * dtm$v)/sum(dtm$v))

exp(-sum(log(colSums(exp(model@beta[,dtm$j])/topics_n)) * dtm$v)/sum(dtm$v))

bet2<-colSums(exp(model@beta[,dtm$j]))




2^(ll/tok)


2^(ll/n_tokens)
2^(-as.numeric(ll/n_tokens))
exp(-as.numeric(ll/n_tokens))

2^(ll/tok)
2^(-as.numeric(ll/tok))
exp(-as.numeric(ll/tok))
exp(as.numeric(ll/n_tokens))





inferencer(mallet_model)
mallet_model$model$getInferencer()
coherence_mat <- data.frame(k = n_topics, 
                            coherence = coherence, 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,70,1)) + ylab("Coherence")

library(SpeedReader)

x <- udpipe::document_term_frequencies(word_counts[, c("doc_id", "word")])
dtm <- udpipe::document_term_matrix(x)
dtm2<-as.matrix(dtm)

lda_mallet<-mallet_lda(documents=dtm2 , iterations = 100, topics=2, hyperparameter_optimization_interval=5,stopword_list = sw,memory = "-Xmx4g",)

kill_zombies()
##############################
library(ldatuning)
system.time({
  tunes <- FindTopicsNumber(
    dtm = dtm,
    topics = c(2:15),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
    method = "Gibbs",
    control = list(seed = 12345),
    mc.cores = 4L,
    verbose = TRUE
  )
})

FindTopicsNumber_plot(tunes)



best.model<-vector()
best.model[1]<-tunes[which.max(tunes$Griffiths2004),1]
best.model[2]<-tunes[which.min(tunes$Arun2010),1]
best.model[3]<-tunes[which.min(tunes$CaoJuan2009),1]
best.model<-mean(best.model)

docs <- mallet.import(txtdf$doc_id,txtdf$text ,stop_words_sbrt)
mallet_model <- MalletLDA(num.topics = best.model)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

documennts_sbrt<-tidy(mallet_model, matrix = "gamma")




loglikh<-mallet_model$model$modelLogLikelihood()

tokens_n<-mallet_model$model$totalTokens

exp(-as.numeric(loglikh/tokens_n))


mallet_eval<-mallet_model$model$getInferencer()
names(txtdf)<-c("id","text")

instance_mallet<-make_instances(txtdf, stoplist_file = stop_words_sbrt)

mod<-m
model_eval<-infer_topics(m,instance_mallet, n_iterations=100)

m2<-load_sampling_state(m)
eval2<-mi_check(m2, 13)

print.mallet_model_inferred(model_eval)
eval2$getSampledDistribution()

mallet_model$model$
model_eval<-mallet_model_inferred(doc_topics=m$doc_topics,
                      doc_ids=m$doc_ids,
                      vocabulary=m$model$getVocabulary(),
                      params=m$params,
                      inf=m,
                      instances=instance_mallet,
                      parent=NULL)

inf<-model_eval$inf(m)


mallet_eval$getSampledDistribution(instance_mallet)

instance_mallet<-make_instances(txtdf, stoplist_file = stop_words_sbrt)

m<-train_model(instance_mallet,n_topics = 13)

dfr_browser(m)






k.topics <- 2:9
folding <- rep(1:5, each = 200)




runonce <- function(k, fold) {
  testing <- which(folding == fold)
  training <- which(folding != fold)
  
  docs <- mallet.import(txtdf$doc_id,txtdf$text ,stop_words_sbrt)
  mallet_model <- MalletLDA(num.topics = best.model)
  mallet_model$loadDocuments(docs)
  mallet_model$train(100)
  
  
  training.docs <- mallet.import(txtdf[training,1], txtdf[training,2], stop_words_sbrt)
  testing.docs <- mallet.import(txtdf[testing,1], txtdf[testing,2], stop_words_sbrt)  
  perplexity(test.model)
}

res <- NULL

for (k in 2:9) {
  for (fold in 1:5) {
    res <- rbind(res, c(k, fold, runonce(k, fold)))
  }
}






###################3

instance_mallet<-make_instances(txtdf, stoplist_file = stop_words_sbrt)

tokens <- txtdf %>%
  unnest_tokens(word, text)


# find document-word counts
word_counts <- tokens %>%
  filter(!(nchar(word) == 1))%>% 
  filter(!word %in% sw) %>%
  count(doc_id, word, sort = TRUE) %>%
  ungroup()




coherence<-vector()
n_topics<-vector()
perplex<-vector()
for (i in 2:100){
  
  m<-train_model(instance_mallet,n_topics = i, metadata = meta2, seed = 12345)
  m$doc_ids<-doc_ids(m)
  

  term_counts <- rename(word_counts, term = word, document = doc_id)
  term_document_sbrt<-augment(m$model, term_counts)
  
  
  x <- udpipe::document_term_frequencies(term_document_sbrt[, c("document", "term")])
  dtm <- udpipe::document_term_matrix(x)
  rm(x)
  
  phi <- mallet.topic.words(m$model)
  theta <- mallet.doc.topics(m$model)
  colnames(dtm) <- 1:ncol(dtm)
  colnames(phi) <- 1:ncol(phi)
  colnames(theta) <- 1:ncol(theta)
  
  coherence.model<-textmineR::CalcProbCoherence(phi = phi, dtm = dtm, M = 5)%>%mean()
  n_topics.model<-m$model$model$numTopics
  loglikh<-m$model$model$modelLogLikelihood()
  tokens_n<-m$model$model$totalTokens
  perplexity.model<-exp(-as.numeric(loglikh/tokens_n))
  
  n_topics[i]<-n_topics.model
  cat(c("Number of topics of the model:",n_topics.model,"\n"))
  
  coherence[i]<-coherence.model
  cat(c("Coherence of the model:",coherence.model,"\n"))
  
  perplex[i]<-perplexity.model
  cat(c("Perplexity of the model:",perplexity.model,"\n"))
  
  cat(c("Remaning",100-i, "traing sets.","\n"))
  
}






