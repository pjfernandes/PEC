setwd("C:/Users/UFF/Desktop/TESTE")

#######################FUNÇÃO pec_pcd()
#######################ELABORADA POR PEDRO JOSÉ FARIAS FERNANDES E RAPHAEL SILVA GIRÃO
#######################INSTALAR OS PACOTES raster e rgdal. PARA ISSO, DIGITE install.packages("nome_do_pacote")
#######################A FUNÇÃO pec() DEVE SER USADA DA SEGUINTE FORMA: pec("nome_do_raster.extensao","nome_do_shapefile.shp")
#######################O SHAPEFILE COM OS PONTOS DE REFERÊNCIA DEVE CONTER O CAMPO NOMEADO COMO z  COM OS VALORES DE ALTIMETRIA.
#######################CONTATO: pj_fernandes@id.uff.br/raphaelgirao@hotmail.com


pec_pcd<-function(mde_leitura,pontos_leitura) {
  
  #CARREGANDO OS PACOTES RASTER E RGDAL
  suppressWarnings(library(raster))
  suppressWarnings(library(rgdal))
  
  
  #DEFININDO O PRIMEIRO ARGUMENTO COMO SENDO O MDE AVALIADO
  mde<-raster(mde_leitura)
  
  #DEFININDO O SEGUNDO ARGUMENTO COMO SHAPE DE PONTOS DE CONTROLE
  shape<-shapefile(pontos_leitura)
  
  
  x<-numeric()
  for(linha in 1:nrow(shape)) {
    x<-c(x,coordinates(shape[linha,])[1,1])
  }
  
  
  y<-numeric()
  for(linha in 1:nrow(shape)) {
    y<-c(y,coordinates(shape[linha,])[1,2])
  }
  
  tabela<-data.frame(x=x,y=y,z=shape$z)
  
  coordinates(tabela)<-c("x","y")
  
  #EXTRAINDO VALORES DO MDE
  valores<-as.vector(extract(mde,tabela))
  
  #JUNTANDO OS VALORES ORIGINAIS COM OS VALORES EXTRAÍDOS DO MDE
  df<-as.data.frame(cbind(tabela$z,valores,abs(valores-tabela$z),(valores-tabela$z)))
  df<-df[complete.cases(df),]
  
 #CÁLCULO TV 1:50000
  classe_50000<-character()
  for (i in df$V3) {
    if(i<=5.5){
      classe_50000<-c(classe_50000,"A")
    } else if (i>5.5 & i<=10){
      classe_50000<-c(classe_50000,"B")
    } else if (i>10 & i<=12){
      classe_50000<-c(classe_50000,"C")
    } else if (i>12 & i<=15) {
      classe_50000<-c(classe_50000,"D") 
    } else {
      classe_50000<-c(classe_50000,"NÃO SE APLICA")
    }
  }
  
  
  df<-cbind(df,classe_50000)
  
  classe_50000_a<-(nrow(df[df$classe_50000=="A",])/nrow(df))*100
  classe_50000_b<-(nrow(df[df$classe_50000=="B",])/nrow(df))*100
  classe_50000_c<-(nrow(df[df$classe_50000=="C",])/nrow(df))*100
  classe_50000_d<-(nrow(df[df$classe_50000=="D",])/nrow(df))*100
  classe_50000_e<-(nrow(df[df$classe_50000=="NÃO SE APLICA",])/nrow(df))*100
  
  #CÁLCULO TV 1:100000
  classe_100000<-character()
  for (i in df$V3) {
    if(i<=13.7){
      classe_100000<-c(classe_100000,"A")
    } else if (i>13.7 & i<=25){
      classe_100000<-c(classe_100000,"B")
    } else if (i>25 & i<=30){
      classe_100000<-c(classe_100000,"C")
    } else if (i>30 & i<=37.5) {
      classe_100000<-c(classe_100000,"D") 
    } else {
      classe_100000<-c(classe_100000,"NÃO SE APLICA")
    }
  }
  
  
  df<-cbind(df,classe_100000)
  
  classe_100000_a<-(nrow(df[df$classe_100000=="A",])/nrow(df))*100
  classe_100000_b<-(nrow(df[df$classe_100000=="B",])/nrow(df))*100
  classe_100000_c<-(nrow(df[df$classe_100000=="C",])/nrow(df))*100
  classe_100000_d<-(nrow(df[df$classe_100000=="D",])/nrow(df))*100
  classe_100000_e<-(nrow(df[df$classe_100000=="NÃO SE APLICA",])/nrow(df))*100
  
  #CÁLCULO TV 1:250000
  classe_250000<-character()
  for (i in df$V3) {
    if(i<=27){
      classe_250000<-c(classe_250000,"A")
    } else if (i>27 & i<=50){
      classe_250000<-c(classe_250000,"B")
    } else if (i>50 & i<=60){
      classe_250000<-c(classe_250000,"C")
    } else if (i>60 & i<=75) {
      classe_250000<-c(classe_250000,"D") 
    } else {
      classe_250000<-c(classe_250000,"NÃO SE APLICA")
    }
  }
  
  
  df<-cbind(df,classe_250000)
  
  classe_250000_a<-(nrow(df[df$classe_250000=="A",])/nrow(df))*100
  classe_250000_b<-(nrow(df[df$classe_250000=="B",])/nrow(df))*100
  classe_250000_c<-(nrow(df[df$classe_250000=="C",])/nrow(df))*100
  classe_250000_d<-(nrow(df[df$classe_250000=="D",])/nrow(df))*100
  classe_250000_e<-(nrow(df[df$classe_250000=="NÃO SE APLICA",])/nrow(df))*100
  
  #CONSOLIDANDO RESULTADO TV E CALCULANDO EMQ
  resultado_TV<-data.frame(escala=c(50000,100000,250000),a=c(classe_50000_a,classe_100000_a,classe_250000_a),b=c(classe_50000_b,classe_100000_b,classe_250000_b),c=c(classe_50000_c,classe_100000_c,classe_250000_c),d=c(classe_50000_d,classe_100000_d,classe_250000_d),nao_se_aplica=c(classe_50000_e,classe_100000_e,classe_250000_e))
  resultado_EMQ<-sqrt(sum(((df$valores-df$V1))^2)/(nrow(df)-1))
  
  #ESCREVENDO RESULTADOS
  print(resultado_TV)
  print("############################")
  print("EMQ")
  print(resultado_EMQ)
  print("############################")
  print("############################")
  print("ESTATÍSTICA DESCRITIVA DA DIFERENÇA RELATIVA")
  print(summary(df$V4))
  print("Desvio Padrão")
  print(sd(df$V4))
  print("############################")
  print("############################")
  print("ESTATÍSTICA DESCRITIVA DA DIFERENÇA ABSOLUTA")
  print(summary(df$V3))
  print("Desvio Padrão")
  print(sd(df$V3))
}

