#faz todo o pre-processamento, tratamento de dados, 
#gera todos as informações necessárias para as visualizações
#e exporta um arquivo .RData para ser importado no dashboard

setwd("/home/micael/R_envs/text-mining/")
source("relatorio/base/get_funs.R")
options(java.parameters="-Xmx6g")
library(dfrtopics)
library(tidytext)
library(tidyverse)
library(mallet)
library(dendextend)

#Diretórios dos arquivos necessários
dossies_dir<-"dados/sbrt_txts/dossies_lematizados_subs"
metadados_dir<-"dados/sbrt_dossies_metadados.json"
stop_words_sbrt<-"dados/stop_words_sbrt.txt"

#Leitura das stopwords.
sw<-readr::read_lines(stop_words_sbrt)

#Leitura e pré-processamento dos documentos.
txtdf<-get_txt(dossies_dir,metadados_dir)

#Criação de um dataframe dos metadados dos documentos.
metadados<-select(txtdf, -text)

#Criação de um datraframe apenas com os documentos e seus respectivos identificadores.
txtdf<-select(txtdf, id, text)

#Preaparação dos documentos para criação do modelo LDA.
instance_mallet<-make_instances(txtdf, stoplist_file = stop_words_sbrt)

#Busca pelo número de tópicos adequado para geração do modelo LDA.
#De acordo com a quantidade de documentos foi definido 60 como numero máximo de tópicos.
best_topic<-find_n_topics(txtdf,instance_mallet,stop_words= sw,metadados, 70)

#Criação do modelo LDA de acordo com o número de tópicos encontrado no processo anterior.
best_model<-train_model(instance_mallet,n_topics = best_topic, metadata = metadados, seed = 12345)
best_model$doc_ids<-doc_ids(best_model)

#Extração dos rótulos dos tópicos.
labels_topics<-topic_labels(best_model, n=5)

#Gera dataframe de documentos por tópico.
documents_sbrt<-tidy(best_model$model, matrix = "gamma")%>%
  arrange(desc(gamma))

#Insere os metadados no dataframe gerado no passo anterior e  seleciona os 100 documentos mais relevantes.
dossies_topic<-metadados %>%
  dplyr::select(id, titulo, categoria, assunto)%>%
  mutate(document=id)%>%
  merge(documents_sbrt)%>%
  select(-id)%>%
  arrange(desc(gamma))%>%
  group_by(topic) %>% slice(1:100)


#Gera dataframe de termos por tópico.
terms_sbrt<-tidy(best_model$model, matrix = "beta")%>%
  arrange(desc(beta))

#Criação do dendrograma de documentos
dendrograma_docs<-cluster.mallet(best_model, method = "ward.D", by="doc")%>%
  as.dendrogram(hang = -1, cex = 0.06)



par(mar = c(0,0,0,0))
dendrograma_docs %>% 
  set("branches_k_color", k = best_topic)%>% 
  set("labels_cex", 1) %>%
  set("leaves_cex",0.2)%>%
  set("hang_leaves",0.2)%>%
  set("branches_lwd", 2)%>%
  plot(horiz=T, axes=F)


#Criação do dendrograma de termos
dendrograma_terms<-cluster.mallet(best_model, method = "ward.D", by="term")%>%
  as.dendrogram(hang = -1, cex = 0.06)



par(mar = c(0,0,0,0))
dendrograma_terms %>% 
  set("branches_k_color", k = best_topic)%>% 
  set("labels_cex", 1) %>%
  set("leaves_cex",0.2)%>%
  set("hang_leaves",0.1)%>%
  set("branches_lwd", 2)%>%
  plot(horiz=T, axes=F)

#Armazena os objetos necessários para a visualização no dashboard.
save(best_topic,dossies_topic,terms_sbrt,dendrograma_docs,dendrograma_terms,metadados, file = "relatorio/base/sbrt_vis_2.RData")


makeLDAvis(best_model$model)
