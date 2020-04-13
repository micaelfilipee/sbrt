get_txt <- function() {
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
  dados<-"/home/micael/R_envs/text-mining/dados/sbrt_txts/dossies"
  txtdf<-readtext::readtext(dados, encoding = "latin1")
  
  metadados<-read_json("/home/micael/R_envs/text-mining/dados/sbrt_dossies_metadados.json")
  
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
  sbrt_sw <- c("http", "senai", "c", "s","b","d","oc","etc", "m", "figura", "or", "p", "al", "et", "l","n","g", "deve","Assunto","responsavel","Nome","Instituicao","acesso","jan","fev","mar","abr","mai","jun","jul","ago","set","nov","dez","Brasileiro", "brasileiro", "brasil", "devem","www.sbrt.ibict.br", "serviço","serviço", "brasileiro", "respostas", "técnicas", "técnico","www.respostatecnica.org.br", "pode", "ser","norma","iso", "kg", "fig", "fonte", "sbrt", "abnt", "nbr", "tecnica",'Copyright',"FIG", "Fonte","SBRT","mm","cm2", "cm")
  sbrt_sw<- iconv(sbrt_sw, from = "UTF-8", to = "ASCII//TRANSLIT")
  #txtdf$text<-stri_replace_all_fixed(txtdf$text,sbrt_sw,"", vectorize_all = FALSE)
  
  
  
  sw_pt_tm <- tm::stopwords("pt") %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  sw_pt_tm <- c(sbrt_sw, sw_pt_tm)
  return(txtdf)
}