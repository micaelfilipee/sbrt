#Funções necessárias para o pre-processamento e geração de visualizações

#Função para importação e pré processamento dos dados

#Recebe como entrada o diretório dos dodumentos e o o caminho dos metadados.
#Retona um dataframe onde cada linha representa um documento e seus respectivos metadados e conteúdo.

get_txt <- function(dados, metadados) {
  # pacote que possibilita a limpeza dos documentos.
  library(stringr)
  library(stringi)
  # pacote TM
  library(tm)
  
  library(SnowballC)
  # pacote que possibilita a tokenizacao
  library(tidytext)
  # pacote de ferramentas de manipulação de dados e geração de graficos.
  library(tidyverse)
  library(formattable)
  # pacote responsavel pela clusterização dos documentos
  library(stm)
  library(ggridges)
  library(jsonlite)
  
  
  #importacao dos dados
  #dados<-"/home/micael/R_envs/text-mining/dados/sbrt_txts/dossies"
  txtdf<-readtext::readtext(dados, encoding = "latin1")
  
  #metadados<-read_json("/home/micael/R_envs/text-mining/dados/sbrt_dossies_metadados.json")
  metadados<-read_json(metadados)
  
  for(i in 1:length(metadados)){
    metadados[[i]][["dossie"]]<-names(metadados[i])
    categorias<-unlist(metadados[[i]][["categoria"]], use.names = F)
    metadados[[i]][["categoria"]]<-NULL
    metadados[[i]][["categoria"]]<-paste(categorias,collapse=",")
    palavras<-unlist(metadados[[i]][["palavras_chave"]], use.names = F)
    metadados[[i]][["palavras_chave"]]<-NULL
    metadados[[i]][["palavras_chave"]]<-paste(palavras,collapse=",")
    rm(categorias)
    rm(palavras)
  }
  
  df_metadados <- data.frame(matrix(unlist(metadados), nrow=length(metadados), byrow=T))
  names(df_metadados)<-names(metadados[[1]])
  df_metadados$doc_id<-df_metadados$dossie
  txtdf$doc_id<-sub('.txt',"",txtdf$doc_id)
  txtdf<-merge(txtdf,df_metadados, by="doc_id")
  txtdf$id<-txtdf$doc_id
  
  ## limpeza
  txtdf$text<-stri_replace_all_fixed(txtdf$text,txtdf$instituicao_responsavel,"", vectorize_all = FALSE)
  #txtdf$text<-stri_replace_all_fixed(txtdf$text,txtdf$data,"", vectorize_all = FALSE)
  txtdf$text<-sub('.*\nConteúdo',"",txtdf$text)
  txtdf$text<-sub('.*\nCONTEÚDO',"",txtdf$text)
  txtdf$text<-sub('.*\fDOSSIÊ TÉCNICO\nTítulo',"",txtdf$text)
  #txtdf$text<-sub('.*\nTítulo',"",txtdf$text)
  txtdf$text<-gsub('[1-9][0-9]* Copyright © Serviço Brasileiro de Respostas Técnicas - SBRT - http://www.sbrt.ibict.br',"",txtdf$text)
  txtdf$text<-gsub('[1-9][0-9]* Copyright © Serviço Brasileiro de Respostas Técnicas - SBRT - http://www.respostatecnica.org.br',"", txtdf$text)
  txtdf$text<-gsub('[1-9][0-9]*\nCopyright © Serviço Brasileiro de Respostas Técnicas - SBRT - http://www.respostatecnica.org.br',"", txtdf$text)
  txtdf$text<-gsub('\nCopyright © Serviço Brasileiro de Respostas Técnicas - SBRT - http://www.sbrt.ibict.br\n\n[1-9][0-9]*',"", txtdf$text)
  txtdf$text<-gsub('c Serviço Brasileiro de Respostas Técnicas',"", txtdf$text)
  txtdf$text<-gsub('Disponível em: ',"",txtdf$text)
  txtdf$text<-gsub('Acesso em: ',"",txtdf$text)
  txtdf$text<-gsub('Nome do responsável',"",txtdf$text)
  txtdf$text<-gsub('Nome da Instituição do responsável',"",txtdf$text)
  txtdf$text<-gsub('Data de finalização',"",txtdf$text)
  txtdf$text<-str_replace_all(txtdf$text, "[^[:alnum:].:,?!;]", " ")
  txtdf$text<-gsub("\\s+", " ", str_trim(txtdf$text))
  txtdf$text<-gsub('Copyright Serviço Brasileiro de Respostas Técnicas SBRT http: www.respostatecnica.org.br [1-9][0-9]*',"",txtdf$text)
  txtdf$text<-gsub('INTRODUÇÃO',"",txtdf$text)
  txtdf$text<-gsub('Introdução',"",txtdf$text)
  txtdf$text<-gsub('http*?br ',"",txtdf$text, perl = T)
  txtdf$text<-gsub('www.*?br ',"",txtdf$text, perl = T)
  txtdf$text<-gsub('pt.*?org wiki',"",txtdf$text)
  txtdf$text<-gsub('^[0-9]+[a-z]+\\b',"",txtdf$text, perl = T)
  txtdf$text<-gsub('^[a-z]\\b'," ",txtdf$text,  perl = T)
  txtdf$text<-gsub('^[\\W]+'," ",txtdf$text, perl = T)
  txtdf$text<-gsub('[0-9]+',"",txtdf$text, perl = T)
  txtdf$text<- iconv(txtdf$text, from = "UTF-8", to = "ASCII//TRANSLIT")
  txtdf$text<-gsub("\\s+", " ", str_trim(txtdf$text))
  #stxtdf$text<-gsub("s[[:punct:]](?=\\s)|s(?=\\s)|s(?=$)","",txtdf$text, perl = T)
  #txtdf$text<-stri_replace_all_fixed(txtdf$text,sbrt_sw,"", vectorize_all = FALSE)
  

  return(txtdf)
}



require(dfrtopics)
require(mallet)
require(ggplot2)
options(java.parameters="-Xmx6g")

#Função necessária para busca do modelo LDA mais adquado.
#Recebe como entrada o dataframe com um documento por linha e seu respectivo identificador,
#uma instancia do pacote Mallet, um dataframe contendo os metadados dos documentos do primeiro dataframe,
#o numero maximo de tópicos.

#Retorna um gráffico demonstrando a pontuação de coerencia para cada modelo e o número de tópcios adequado.

find_n_topics<-function(txtdf,instance_mallet,stop_words,metadata, max_topics){
  
  tokens <- txtdf %>%
    tidytext::unnest_tokens(word, text)
  
  # find document-word counts
  word_counts <- tokens %>%
    filter(!(nchar(word) == 1))%>% 
    filter(!word %in% stop_words) %>%
    count(id, word, sort = TRUE) %>%
    ungroup()
  
  coherence<-vector()
  n_topics<-vector()
  
  for (i in seq(2,max_topics,2)){
    
    m<-train_model(instance_mallet,n_topics = i, metadata = metadata, seed = 12345)
    m$doc_ids<-doc_ids(m)
    
    
    term_counts <- rename(word_counts, term = word, document = id)
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
    
    n_topics[i]<-n_topics.model
    cat(c("Number of topics of the model:",n_topics.model,"\n"))
    
    coherence[i]<-coherence.model
    cat(c("Coherence of the model:",coherence.model,"\n"))
    
    cat(c("Remaning",max_topics-i, "traing sets.","\n"))
    gc()
    
  }
  
  coherence_mat <- data.frame(k = na.omit(n_topics), 
                              coherence = na.omit(coherence), 
                              stringsAsFactors = FALSE)
  p<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
    geom_point() +
    ggtitle("Melhor Tópico pela Coerência") + 
    theme_minimal() +
    scale_x_continuous(breaks = seq(2,max_topics,2)) + 
    ylab("Coerência")+
    xlab("Tópico")
  best.model.k<-coherence_mat[which.max(coherence_mat$coherence),1]
  
  print(p)
  
  return(best.model.k)
}



#Função necessária para a geração da visualização do LDAvis.

#Recebe como entrada um modelo gerado pelo pacote RMALLET ou DFRTOPICS.
#Opcionalmente pode-se indicar um diretório de saída para os arquivos responsáveis pela interface web da ferramenta.
#Para isso, basta indicar o diretório da seguinte maneira (outDir="diretorio_desejado")

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
  library(gistr)
  if(asGist==TRUE) library(gistr)
  LDAvis::serVis(jsonLDA, out.dir = outDir, open.browser = openBrowser, as.gist = asGist, ... = ...)
}



#Função necessária para a geração do dendrograma a partir do modelo LDA gerado pelo pacote Mallet.

#Recebe como entrada o modelo LDA, valor balance, onde 0 representa um cluster de acordo com os documentos e
#1 representa um cluster de acordo com os termos e, method, o qual determinará qual será o tipo de metodo
#de custerização: ward.D, Ward.D2, single, complete, average, mcquitty, median ou centroid.

#Retorna um objeto da classe hclust.
cluster.mallet<-function (model, balance = NULL, method=NULL, by=NULL) 
{
  doc.topics<-mallet.doc.topics(model$model)
  topic.words<-mallet.topic.words(model$model)
  topic.labels<-topic_labels(model, n=4)
  topic.docs <- t(doc.topics)
  topic.docs <- topic.docs/rowSums(topic.docs)
  #clus<-hclust(balance * dist(topic.words) + (1 - balance) * dist(topic.docs),method,"ave")
  if (by=="doc"){
    clus<-hclust(dist(topic.docs),method)

  }
  if (by=="term"){
    clus<-hclust(dist(topic.words),method)

  }
  clus$labels<-topic.labels
  return(clus)
}

