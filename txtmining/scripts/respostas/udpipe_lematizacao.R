library(dplyr)
library(udpipe)
library(pdftools)
library(httr)


ud_model <- udpipe_download_model(language = "portuguese")
ud_model <- udpipe_load_model(ud_model$file_model)

setwd("/home/micael/R_envs/text-mining/")
source("scripts/respostas/get_funs.R")
#Diretŕoio das respostas em pdf
dir_respostas<-"/home/micael/Desktop/pdf_sbrt/RT"
#Diretório onde os txts das respostas lematizadas serão salvos
dir_respostas_lemma<-"/home/micael/R_envs/text-mining/dados/sbrt_txts/respostas_lematizadas"
#Diretŕoio onde os txts serão salvos sem lematização
dir_respostas_txt<-"/home/micael/R_envs/text-mining/dados/sbrt_txts/respostas_txt"

#Listando todos os aquivos do diretório dir_respostas
files_respostas<- list.files(dir_respostas,full.names = TRUE)

for (i in 1:length(files_respostas)){
  resposta<-files_respostas[i]
  #Conversão de pdf para txt
  dftxt<-txt_resposta_pdf(resposta)
  if(dftxt!= "erro"){
    #Tokenização e lematização do texto 
    x <- udpipe_annotate(ud_model, x = dftxt$text, doc_id = dftxt$doc_id)
    x <- as.data.frame(x)
    #Selecionando apenas as sentenças
    x2<-select(x,  doc_id,paragraph_id,sentence_id,sentence)%>%unique()
    #Gravanddo as sentenças no banco de dados
    for (i in 1:length(x2$doc_id)){
      
      dados<-list(p_id = x2[i,2],
      sentenca_id = x2[i,3],
      sentenca = x2[i,4])
      api<-paste0("http://192.168.2.7:8282/resposta/",x2[i,1])
      res <- POST(api, body = dados, encode = "json", verbose())
    }
    #Salvando o texto lematizado
    txt_lema<-lemmaToDf(x)
    write(txt_lema$text,file = paste0(dir_respostas_lemma,"/",txt_lema$doc_id,".txt"))
    #Salvando não lematizado
    write(as.character(dftxt$text),file = paste0(dir_respostas_txt,"/",dftxt$doc_id,".txt"))

}

  }




















