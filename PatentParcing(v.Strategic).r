library(tm)
library(ggplot2)
library(lsa)
library(scatterplot3d)
library(SnowballC)
library(PARSE)
library(httr)
library(XML)
library(RCurl)
library(rlist)
library(odbc)
library(rvest)
library(RSelenium)
library(wordcloud2)
library(igraph)
library(networkD3)
library(plotly)
library(WordR)
library(knitr)
library(openxlsx)
library(cluster)
library(ClusterR)
library(topicmodels)
library(ggrepel)
library(parallel)
library(foreach)
library(fastTextR)

# Calculate the number of cores
#no_cores <- detectCores() - 1

# Initiate cluster
#cl <- makeCluster(no_cores)
#foreach %dopar% все циклы вайл можно переписать - останется вопрос с LSA и кластеризацией
#stopCluster(cl)

getwd()


#####Функции#####
DpatentlistYearSt = function(Year) {
  PV=0
  nad=1
  DpatentlistYear=data.frame(NULL)
  colpat=length(Dpatentlist)
  while(nad<=colpat)
  {
    if(as.numeric(Dpatentlist[3,nad])==Year)
    {
      PV=PV+1
      i=1
      while(i<=3)
      {
        DpatentlistYear[i,PV]=Dpatentlist[i,nad]
        i=i+1
      }
      DpatentlistYear[4,PV]=nad
      DpatentlistYear[5,PV]=Dpatentlist[5,nad]
    }
    nad=nad+1
    setTxtProgressBar(ProgressBar, nad/colpat*100)
  }
  return(DpatentlistYear)
}

YearPatents = function(DpatentlistYear) { 
  #связи между выбранными патентами
  Ypatentgraph = data.frame(NULL)
  Yfrom = c(NULL)
  Yto = c(NULL)
  YNfrom = c(NULL) #для ни с чем не связанных патентов 
  YNto = c(NULL)
  notmatched=0
  nad=1
  colpat=length(DpatentlistYear)
  while(nad<=colpat)
  {
    words = strsplit(DpatentlistYear[1,nad], ' ')
    words[[1]]=removePunctuation(words[[1]])
    words[[1]]=removeNumbers(words[[1]])
    #words[[1]]=removeWords(words[[1]], c('^ing$', '^ity$','^ying$', '^sion$', '^ding$', '^king$')) #удаляем стоп-слова
    words[[1]]=removeWords(words[[1]], stopwords("english"))
    words[[1]]=removeWords(words[[1]], ObjectDictionary)
    words[[1]]=removeWords(words[[1]], AdStops)
    words[[1]] = tolower(words[[1]])
    words[[1]]=unique(words[[1]])
    nw=1
    nwm=length(words[[1]])
    while(nw<=nwm)
    {
      if(is.na(words[[1]][nw]))
        words[[1]][nw]=''
      if(nchar(words[[1]][nw])>2)
      {
        used=1
        gw=unique(grep(words[[1]][nw],tolower(DpatentlistYear[1,]), fixed=TRUE))
        if(is.null(length(gw))) #на тот случай, если пустая строка - нет совпадений
        {
          length(gw)=0
          notmatched=notmatched+1
          YNfrom = c(YNfrom, nad)
          YNto = c(YNto, nad)
        }
        if(length(gw)>0)
        {
          while(used<=length(gw))
          {
            if(gw[used]>nad)
              break
            used=used+1
          }
          if(used<=length(gw))
          {
            Yto = c(Yto, gw[used:length(gw)])
            Yfrom = c(Yfrom, rep(nad,length(gw)-used+1))
          }
        }
      }
      nw=nw+1
      setTxtProgressBar(ProgressBar1, nw/nwm*100)
    }
    nad=nad+1
    print(nad)
    flush.console()
  }
  return(data.frame(c(Yfrom,YNfrom),c(Yto,YNto),stringsAsFactors = FALSE))
}

YearPatentsOld = function(DpatentlistYear) { 
  #связи между выбранными патентами
  Ypatentgraph = data.frame(NULL)
  Yfrom = c(NULL)
  Yto = c(NULL)
  YNfrom = c(NULL) #для ни с чем не связанных патентов 
  YNto = c(NULL)
  notmatched=0
  nad=1
  colpat=length(DpatentlistYear)
  while(nad<=colpat)
  {
    words = strsplit(DpatentlistYear[1,nad], ' ')
    words[[1]]=removePunctuation(words[[1]])
    words[[1]]=removeNumbers(words[[1]])
    #words[[1]]=removeWords(words[[1]], c('^ing$', '^ity$','^ying$', '^sion$', '^ding$', '^king$')) #удаляем стоп-слова
    words[[1]]=removeWords(words[[1]], stopwords("english"))
    words[[1]]=removeWords(words[[1]], ObjectDictionary)
    words[[1]]=removeWords(words[[1]], AdStops)
    words[[1]] = tolower(words[[1]])
    words[[1]]=unique(words[[1]])
    nw=1
    nwm=length(words[[1]])
    while(nw<nwm)
    {
      if(is.na(words[[1]][nw]))
        words[[1]][nw]=''
      if(nchar(words[[1]][nw])>2)
      {
        used=1
        gw=unique(grep(words[[1]][nw],tolower(DpatentlistYear[1,]), fixed=TRUE))
        gw1=unique(grep(words[[1]][nw+1],tolower(DpatentlistYear[1,]), fixed=TRUE))
        #убрать из gw то, чего нет в gw1
        gws=c(NULL)
        gwi=1
        gwm=length(gw)
        while(gwi<=gwm)
        {
          gw1i=1
          gw1m=length(gw1)
          while(gw1i<=gw1m)
          {
            if(gw[gwi]==gw1[gw1i])
              gws=c(gw, gw[gwi]) # исправление алгоритма с gws = gw[gwi]
            gw1i=gw1i+1
          }
          gwi=gwi+1
        }
        if(is.null(length(gws))) #на тот случай, если пустая строка - нет совпадений
        {
          length(gws)=0
          notmatched=notmatched+1
          YNfrom = c(YNfrom, nad)
          YNto = c(YNto, nad)
        }
        if(length(gws)>0)
        {
          while(used<=length(gws))
          {
            if(gws[used]>nad)
              break
            used=used+1
          }
          if(used<=length(gws))
          {
            Yto = c(Yto, gws[used:length(gws)])
            Yfrom = c(Yfrom, rep(nad,length(gws)-used+1))
          }
        }
      }
      nw=nw+1
      setTxtProgressBar(ProgressBar1, nw/nwm*100)
    }
    nad=nad+1
    print(nad)
    print(length(gws))
    flush.console()
  }
  return(data.frame(c(Yfrom,YNfrom),c(Yto,YNto),stringsAsFactors = FALSE))
}

YearPatents1 = function(DpatentlistYear) {
  Ypatentgraph = data.frame(NULL)
  Yfrom = c(NULL)
  Yto = c(NULL)
  YNfrom = c(NULL) #для ни с чем не связанных патентов 
  YNto = c(NULL)
  Ydist.mat = data.frame(NULL)
  i=1
  im=length(DpatentlistYear)
  jm=length(DpatentlistYear)
  while(i<=im)
  {
    j=1
    while(j<=jm)
    {
      Ydist.mat[i,j] = dist.mat[DpatentlistYear[4,i], DpatentlistYear[4,j]]
      j=j+1
    }
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  Yweight = c(NULL)
  YNweight = c(NULL)
  i=1
  im=length(Ydist.mat[1,])
  jm=im
  while(i<=im)
  {
    j=1
    nmflag = 0
    while(j<=jm)
    {
      if(i<j)
      {
        if(Ydist.mat[i,j] <= ConDist)
        {
          Yfrom[length(Yfrom)+1] = i
          Yto[length(Yto)+1] = j
          Yweight[length(Yweight)+1] = Ydist.mat[i,j]
          nmflag = 1
        }
      }
      j=j+1
    }
    if(nmflag == 0)
    {
      YNfrom[length(YNfrom)+1] = i
      YNto[length(YNto)+1] = i
      YNweight[length(YNweight)+1] = 0
    }
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  return(data.frame(c(Yfrom,YNfrom),c(Yto,YNto), c(Yweight, YNweight), stringsAsFactors = FALSE))
}

conweightening = function(){
  YConWeight = c(NULL)
  i=1
  im=length(Ytemppg[,1])
  while(i<=im)
  {
    YConWeight[i]=Ytemppg[i,3] #length(Ytemppg[,1][Ytemppg[,2]==Ypatentgraph[i,2]][Ytemppg[,1][Ytemppg[,2]==Ypatentgraph[i,2]]==Ypatentgraph[i,1]])
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  return(YConWeight)
}

YMacros = function(){
  #вставка внешних трендов всех уровней
  tr=1
  #trm=length(trends)
  trm=length(trendsSM)
  YMto = c(NULL)
  YMfrom = c(NULL)
  DpY = stemDocument(as.character(DpatentlistYear[5,]), language = 'english')
  while(tr<=trm)
  {
    #gw=unique(grep(trends[tr],DpatentlistYear[1,], fixed=TRUE))
    #gw=unique(grep(trendsSM[tr],DpatentlistYear[5,], fixed=TRUE))
    trendsSM[6]
    gw=unique(grep(trendsSM[tr],DpY, fixed=TRUE))
    if(is.null(length(gw))) #на тот случай, если пустая строка
      length(gw)=0
    if(length(gw)>=1)
    {
      YMto = c(YMto, gw)
      YMfrom = c(YMfrom, rep(MMtrends[tr],length(gw)))
    }
    tr=tr+1
    setTxtProgressBar(ProgressBar1, tr/trm*100)
  }
  return((data.frame(c(YMfrom),c(YMto),stringsAsFactors = FALSE)))
}

creategraph = function(Mfrom, Mto) {
  #граф патентов
  actors <- unique(Ypatentgraph["From"])
  clearC <- data.frame(Ypatentgraph["From"], Ypatentgraph["To"])
  g <- graph_from_data_frame(unique(clearC), directed=FALSE, vertices=NULL)
  set.seed(42)
  i <- 1
  n <- length(unique(clearC)[1])
  type <- rep(rgb(as.numeric(t(col2rgb("gold"))[1,1])/255, as.numeric(t(col2rgb("gold"))[1,2])/255, as.numeric(t(col2rgb("gold"))[1,3])/255, alpha=0.5),n)
  alpha <- rep(0.01,n)
  # Выбираем алгоритм для ракладки графа (Kamada-Kawai)
  l3 <- layout.fruchterman.reingold(g)
  # Вершины сети будут в форме круга
  V(g)$shape <- "circle"
  # Задаем цвета вершин и ребер между ними.
  # Раскрасим вершины в цвета групп, к которым приписаны сотрудники университета.
  E(g)$color <- type
  E(g)$alpha <- alpha
  i<-1
  length(V(g)$name)
  V(g)$name[2]
  i=length(unique(clearC)[,1]) - length(unique(data.frame(a=Mfrom, b=Mto))[,1])+1
  is=i
  im=length(unique(clearC)[,1])
  im
  E(g)$color[314]
  while(i<=im)
  {
    E(g)$color[i]=rgb(as.numeric(t(col2rgb("red"))[1,1])/255, as.numeric(t(col2rgb("red"))[1,2])/255, as.numeric(t(col2rgb("red"))[1,3])/255, alpha=0.5)
    i=i+1
    setTxtProgressBar(ProgressBar1, (i-is)/(im-is)*100)
  }
  #Раскраска по тематикам
  i=1
  im=length(unique(Ypatentgraph[,1]))-length(unique(Mfrom))
  while(i<=im)
  {
    V(g)$color[i] = rgb(as.numeric(t(col2rgb(clasters[as.numeric(DpatentlistYear[4,as.numeric(V(g)$name[i])]),1]))[1,1])/255, as.numeric(t(col2rgb(clasters[as.numeric(DpatentlistYear[4,as.numeric(V(g)$name[i])]),1]))[1,2])/255, as.numeric(t(col2rgb(clasters[as.numeric(DpatentlistYear[4,as.numeric(V(g)$name[i])]),1]))[1,3])/255, alpha=0.5)
    V(g)$name[i] = DpatentlistYear[4,i]
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  i1=length(unique(Ypatentgraph[,1]))
  i=1
  im = length(unique(c(Ypatentgraph[,1],Ypatentgraph[,1])))-length(unique(Ypatentgraph[,1]))
  while(i<=im)
  {
    i1=i1+1
    V(g)$color[i1] = rgb(as.numeric(t(col2rgb(clasters[as.numeric(DpatentlistYear[4,as.numeric(V(g)$name[i1])]),1]))[1,1])/255, as.numeric(t(col2rgb(clasters[as.numeric(DpatentlistYear[4,as.numeric(V(g)$name[i1])]),1]))[1,2])/255, as.numeric(t(col2rgb(clasters[as.numeric(DpatentlistYear[4,as.numeric(V(g)$name[i1])]),1]))[1,3])/255, alpha=0.5)
    V(g)$name[i1]=DpatentlistYear[4,i1]
    i=i+1  
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  # Уменьшим размеры и толщину стрелок ребер.
  E(g)$arrow.size <- 0
  E(g)$arrow.width <- 0
  # Размер вершины пропорционален ее степени центральности.
  # Берем квадратный корень из степени центральности.
  V(g)$size = graph.strength(g)^(1/20)
  V(g)$label <- V(g)$name
  V(g)$alpha <- alpha
  E(g)$width = 1
  i=1
  im=length(YConWeight)
  while(i<=im)
  {
    E(g)$width[i]=YConWeight[i]*20
    if(E(g)$width[i]==0)
      E(g)$width[i] = 0.0000001
    i=i+1
  }
  E(g)$width
  YConWeight*10
  # Визуализируем нашу сеть
  plot(g, layout=l3)
}

Ypthemegraph = function(){
  Ythemegraph = data.frame(NULL)
  YTfrom = c(NULL)
  YTto = c(NULL)
  YTConWeight = c(NULL)
  pg = 1
  pgm = length(unique(Ypatentgraph)[,1])-length(unique(YMacro)[,1])
  while(pg<=pgm)
  {
    YTfrom[length(YTfrom)+1] = clasters[DpatentlistYear[4,as.numeric(Ypatentgraph[pg,1])],1]
    YTto[length(YTto)+1] = clasters[DpatentlistYear[4,as.numeric(Ypatentgraph[pg,2])],1]
    pgflag = 0
    if(YTfrom[length(YTfrom)] != YTto[length(YTto)])
    {
      YTConWeight[length(YTConWeight)+1] = as.numeric(Ytemppg[pg,3])
      pgflag = 1
    }
    if(pgflag == 0)
    {
      YTConWeight[length(YTConWeight)+1] = 0
    }
    pg=pg+1
    setTxtProgressBar(ProgressBar1, pg/pgm*100)
  }
  max(clasters)
  Ythemegraph = unique(data.frame(From = YTfrom, To = YTto, Weight = YTConWeight, stringsAsFactors = FALSE))
  return(Ythemegraph)
}

YTMacros=function(){
  #связь тематик и макротрендов
  YTMfrom = c(NULL)
  YTMto = c(NULL)
  i=1
  im=length(YMacro[,1])
  while(i<=im)
  {
    YTMto=c(YTMto, clasters[DpatentlistYear[4,YMacro[i,2]],1])
    YTMfrom = c(YTMfrom,as.character(YMacro[i,1]))
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  YthemegraphM = data.frame(From = YTMfrom, To = YTMto,stringsAsFactors = FALSE)
  return(YthemegraphM)
}

tconweightening = function(){
  YTConWeight = c(NULL)
  i=1
  im=length(Ythemegraph[,1])
  if(length(unique(YthemegraphM)[1,])>0)
    im=length(Ythemegraph[,1])-length(unique(YthemegraphM)[,1])
  while(i<=im)
  {
    S = sum(Ytemptg[,3][Ytemptg[,2]==Ythemegraph[i,2]][Ytemptg[,1][Ytemptg[,2]==Ythemegraph[i,2]]==Ythemegraph[i,1]])
    if(S == 0)
      YTConWeight[i]=0
    if(S > 0)
      YTConWeight[i]=sum(Ytemptg[,3][Ytemptg[,2]==Ythemegraph[i,2]][Ytemptg[,1][Ytemptg[,2]==Ythemegraph[i,2]]==Ythemegraph[i,1]]) / length(Ytemptg[,3][Ytemptg[,2]==Ythemegraph[i,2]][Ytemptg[,1][Ytemptg[,2]==Ythemegraph[i,2]]==Ythemegraph[i,1]])
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  length(Ytemptg[,1])
  length(Ytemptg[,3])
  length(Ythemegraph[,1])
  return(YTConWeight)
}

TWeight = function() {
  #определение веса тематик
  Ytn=1
  YThemeC=data.frame(NULL)
  #Ytnm = length(lsaSpace$s)
  Ytnm = KCounter
  nadm = length(DpatentlistYear)
  while(Ytn<=Ytnm)
  {
    nad=1
    Ytcount=0
    while(nad<=nadm)
    {
      if(clasters[DpatentlistYear[4,nad],1]==Ytn)
      {
        Ytcount=Ytcount+1
      }
      nad=nad+1
    }
    YThemeC[Ytn,1] = Ytcount
    Ytn=Ytn+1
    setTxtProgressBar(ProgressBar1, Ytn/Ytnm*100)
  }
  return(YThemeC)
}

createTgraph=function(){
  #визуализация графа связи тематик
  actors <- unique(Ythemegraph["From"])
  clearC <- data.frame(Ythemegraph["From"], Ythemegraph["To"])
  g <- graph_from_data_frame(unique(clearC), directed=FALSE, vertices=NULL)
  unique(clearC)
  set.seed(42)
  i <- 1
  n <- length(unique(clearC)[,1])
  type <- rep(rgb(as.numeric(t(col2rgb("gold"))[1,1])/255, as.numeric(t(col2rgb("gold"))[1,2])/255, as.numeric(t(col2rgb("gold"))[1,3])/255, alpha=0.5),n)
  alpha <- rep(0.5,n)
  # Выбираем алгоритм для ракладки графа (Kamada-Kawai)
  #l <- layout.sphere(g) #layout.sphere(g)
  #l1 <- layout.fruchterman.reingold(g)
  #l2 <- layout.kamada.kawai(g)
  #l3 = layout.lgl(g)
  #l3=layout.auto(g)
  l3 <- layout.fruchterman.reingold(g)
  # Вершины сети будут в форме круга
  V(g)$shape <- "circle"
  # Задаем цвета вершин и ребер между ними.
  # Раскрасим вершины в цвета групп, к которым приписаны сотрудники университета.
  E(g)$color <- type
  E(g)$alpha <- alpha
  i=length(unique(clearC)[,1]) - length(unique(data.frame(a=YthemegraphM[,1], b=YthemegraphM[,2]))[,1])+1
  is=i
  im=length(unique(clearC)[,1])
  im
  E(g)$color[314]
  while(i<=im)
  {
    E(g)$color[i]=rgb(as.numeric(t(col2rgb("red"))[1,1])/255, as.numeric(t(col2rgb("red"))[1,2])/255, as.numeric(t(col2rgb("red"))[1,3])/255, alpha=0.5)
    i=i+1
    setTxtProgressBar(ProgressBar1, (i-is)/(im-is)*100)
  }
  i=1
  im=length(unique(clearC)[,1]) - length(unique(data.frame(a=YthemegraphM[,1], b=YthemegraphM[,2]))[,1])
  while(i<=im)
  {
    if(as.numeric(clearC[i,1]) > HThemeC[1] && as.numeric(clearC[i,2]) > HThemeC[1])
    {
      E(g)$color[i] = rgb(as.numeric(t(col2rgb("green"))[1,1])/255, as.numeric(t(col2rgb("green"))[1,2])/255, as.numeric(t(col2rgb("green"))[1,3])/255, alpha=0.5)
    }
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  length(V(g)$name)
  V(g)$name[2]
  #Раскраска по тематикам
  V(g)$color = rep(rgb(.193,.0,.32, alpha = 0.5),length(V(g)))
  i=1
  im=length(unique(Ythemegraph[,1]))-length(unique(YthemegraphM[,1]))
  while(i<=im)
  {
    V(g)$color[i] = rgb(as.numeric(t(col2rgb(V(g)$name[i]))[1,1])/255, as.numeric(t(col2rgb(V(g)$name[i]))[1,2])/255, as.numeric(t(col2rgb(V(g)$name[i]))[1,3])/255, alpha=0.5)
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  i1=length(unique(Ythemegraph[,1]))
  i=1
  im = length(unique(c(Ythemegraph[,1],Ythemegraph[,2])))-length(unique(Ythemegraph[,1]))
  while(i<=im)
  {
    i1=i1+1
    V(g)$color[i1] = rgb(as.numeric(t(col2rgb(V(g)$name[i1]))[1,1])/255, as.numeric(t(col2rgb(V(g)$name[i1]))[1,2])/255, as.numeric(t(col2rgb(V(g)$name[i1]))[1,3])/255, alpha=0.5)
    i=i+1  
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  V(g)$color
  V(g)$name
  # Уменьшим размеры и толщину стрелок ребер.
  E(g)$arrow.size <- 0
  E(g)$arrow.width <- 0
  # Размер вершины пропорционален ее степени центральности.
  # Берем квадратный корень из степени центральности.
  V(g)$size = rep(3,length(V(g)))
  
  V(g)$name[3]
  
  i=1
  im=length(unique(Ythemegraph[,1]))-length(unique(YthemegraphM[,1]))
  while(i<=im)
  {
    V(g)$size[i] = (YThemeC[V(g)$name[i],1]+1)^1/20
    i=i+1  
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  i1=length(unique(Ythemegraph[,1]))
  i=1
  im = length(unique(c(Ythemegraph[,1],Ythemegraph[,2])))-length(unique(Ythemegraph[,1]))
  while(i<=im)
  {
    i1=i1+1
    V(g)$size[i1] = (YThemeC[V(g)$name[i1],1]+1)^1/20
    i=i+1 
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  if(ThemeGraphFlag == 1)
  {
    im = length(grthemes[,1])
    j=1
    jm=length(V(g))
    test=0
    while(j<=jm)
    {
      i=1
      while(i<=im)
      {
        if(V(g)$name[j] == as.character(grthemes[i,1]))
        {
          V(g)$size[j] = 2
          if(grthemes[i,3] < 1)
            V(g)$shape[j] = "square"
          if(grthemes[i,3] == 1)
            V(g)$shape[j] = "square"
          test = test+1
        }
        i=i+1
      }
      j=j+1
    }
  }
  if(ThemeGraphFlag == 2)
  {
    i=1
    im = length(BRgroups)
    while(i<=im)
    {
      j=1
      jm=length(BRgroups[[i]])
      while(j<=jm)
      {
        V(g)[V(g)$name == as.character(BRgroups[[i]][j])]$shape = "square"
        V(g)[V(g)$name == as.character(BRgroups[[i]][j])]$size = 2
        j=j+1
      }
      i=i+1
    }
    i=1
    im = length(GTNTgroups)
    while(i<=im)
    {
      j=1
      jm=length(GTNTgroups[[i]])
      while(j<=jm)
      {
        V(g)[V(g)$name == as.character(GTNTgroups[[i]][j])]$shape = "square"
        V(g)[V(g)$name == as.character(GTNTgroups[[i]][j])]$size = 2
        j=j+1
      }
      i=i+1
    }
  }
  n1 <- length(graph.strength(g))
  V(g)$label <- V(g)$name
  V(g)$alpha <- rep(0.5, length(V(g)))
  E(g)$lty=rep(1,length(E(g)))                            # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
  E(g)$curved=rep(0,length(E(g))) 
  E(g)$width = 1
  i=1
  im=length(YTConWeight)
  while(i<=im)
  {
    E(g)$width[i]=YTConWeight[i]
    if(E(g)$width[i] == 0)
      E(g)$width[i] = 0.000001
    i=i+1
  }
  length(YTConWeight)
  YTConWeight
  length(E(g)$width)
  E(g)$width
  clearC
  V(g)$size[3]=1
  # Визуализируем нашу сеть
  if(ThemeGraphFlag == 0)
  {
    plot(g, layout=l3)
  }
  if(ThemeGraphFlag == 1)
  {
    grlistc = c(NULL)
    grlists = c(NULL)
    j=1
    im=length(grthemes[,1])
    jm = length(V(g))
    while(j<=jm)
    {
      i=1
      while(i<=im)
      {
        if(V(g)$name[j] == as.character(grthemes[i,1]))
        {
          if(grthemes[i,3] < 1)
            grlistc = c(grlistc,j)
          if(grthemes[i,3] == 1)
            grlists = c(grlists,j)
        }
        i=i+1
      }
      j=j+1
    }
    #выделить группы
    A = rgb(as.numeric(t(col2rgb("blue"))[1,1])/255, as.numeric(t(col2rgb("blue"))[1,2])/255, as.numeric(t(col2rgb("blue"))[1,3])/255, alpha=0.2)
    B = rgb(as.numeric(t(col2rgb("pink"))[1,1])/255, as.numeric(t(col2rgb("pink"))[1,2])/255, as.numeric(t(col2rgb("pink"))[1,3])/255, alpha=0.2)
    plot(g, layout=l3,mark.groups=list(grlistc, grlists), mark.col=c(A,B), mark.border=NA)
  }
  if(ThemeGraphFlag == 2)
  {
    MarkList = list(NULL)
    MarkVol = 0
    i=1
    im=length(BRgroups)
    while(i<=im)
    {
      MarkVol = MarkVol +1
      MarkList[[MarkVol]] = c(NULL)
      j=1
      jm=length(BRgroups[[i]])
      while(j<=jm)
      {
        o=1
        om = length(V(g))
        while(o<=om)
        {
          if(V(g)$name[o] == as.character(BRgroups[[i]][j]))
          {
            if(j==1)
              MarkList[[MarkVol]] = o
            if(j>1)
              MarkList[[MarkVol]][j] = o
          }
          o=o+1
        }
        j=j+1
      }
      i=i+1
    }
    AN = MarkVol
    i=1
    im=length(GTNTgroups)
    while(i<=im)
    {
      MarkVol = MarkVol +1
      MarkList[[MarkVol]] = c(NULL)
      j=1
      jm=length(GTNTgroups[[i]])
      while(j<=jm)
      {
        o=1
        om = length(V(g))
        while(o<=om)
        {
          if(V(g)$name[o] == as.character(GTNTgroups[[i]][j]))
          {
            if(j==1)
              MarkList[[MarkVol]] = o
            if(j>1)
              MarkList[[MarkVol]][j] = o
          }
          o=o+1
        }
        j=j+1
      }
      i=i+1
    }
    BN = MarkVol - AN
    #выделить группы
    A = rgb(as.numeric(t(col2rgb("blue"))[1,1])/255, as.numeric(t(col2rgb("blue"))[1,2])/255, as.numeric(t(col2rgb("blue"))[1,3])/255, alpha=0.2)
    B = rgb(as.numeric(t(col2rgb("pink"))[1,1])/255, as.numeric(t(col2rgb("pink"))[1,2])/255, as.numeric(t(col2rgb("pink"))[1,3])/255, alpha=0.2)
    plot(g, layout=l3,mark.groups=MarkList, mark.col=c(rep(A,AN),rep(B,BN)), mark.border=NA)
  }
}

macroconnect = function() {
  TMC=data.frame(NULL)
  tn=1
  while(tn<=KCounter)
  {
    sl = length(YthemegraphM[,2])
    ts=1
    tmcount = 0
    while(ts<=sl)
    {
      if(as.numeric(YthemegraphM[ts,2])==tn)
        tmcount=tmcount+1
      ts=ts+1
    }
    TMC[tn,1] = tn
    TMC[tn,2] = tmcount
    setTxtProgressBar(ProgressBar1, tn/KCounter*100)
    tn=tn+1
  }
  return(TMC)
}
themeconnect = function(){
  #TMC=data.frame(NULL)
  TMC = matrix(rep(0,KCounter*KCounter), nrow = KCounter, ncol = KCounter)
  YthemegraphM = unique(YthemegraphM)
  i=1
  im = length(c((Ythemegraph[1:(length(Ythemegraph[,1])-length(YthemegraphM[,1])),1])))
  while(i<=im)
  {
    TMC[as.numeric(Ythemegraph[i,1]), as.numeric(Ythemegraph[i,2])] = YTConWeight[i]
    TMC[as.numeric(Ythemegraph[i,2]), as.numeric(Ythemegraph[i,1])] = YTConWeight[i]
    i=i+1
  }
  TMC[TMC == 'NA']
  return(TMC)
}

Analytics = function(Times){
  V=c(NULL)
  V[1]=Year
  tn=1
  nadm=length(DpatentlistYear)
  while(tn<=KCounter)
  {
    nad=1
    tcount=0
    while(nad<=nadm)
    {
      if(clasters[DpatentlistYear[4,nad],1]==tn)
      {
        tcount=tcount+1
      }
      nad=nad+1
    }
    V[tn+1] = tcount
    tn=tn+1
    setTxtProgressBar(ProgressBar1, tn/KCounter*100)
  }
  Mad=1
  Madm = length(MMtrends)
  while(Mad<=Madm)
  {
    gw=unique(grep(trendsSM[Mad],DpatentlistYear[5,], fixed=TRUE))
    if(is.null(length(gw))) #на тот случай, если пустая строка
      length(gw)=0
    V[tn+Mad]=length(gw)
    Mad=Mad+1
    setTxtProgressBar(ProgressBar1, Mad/Madm*100)
  }
  return(V)
}

AnalyticsPro = function(VV){
  V=c(NULL)
  V[1]=Year
  tn=1
  a=tn+1
  b=KCounter+1
  SumVV = sum(VV[a:b])
  while(tn<=KCounter)
  {
    V[tn+1] = VV[tn+1]/SumVV
    tn=tn+1
    setTxtProgressBar(ProgressBar1, tn/KCounter*100)
  }
  Mad=1
  length(Mtrends)
  a=tn+Mad
  b=length(lsaSpace$s)+1+length(MMtrends)
  SumTVV = sum(VV[as.numeric(tn+Mad):as.numeric(KCounter+1+length(Mtrends))])
  while(Mad<=length(MMtrends))
  {
    V[tn+Mad]=VV[tn+Mad]/SumTVV
    Mad=Mad+1
    setTxtProgressBar(ProgressBar1, Mad/length(MMtrends)*100)
  }
  return(V)
}

#по определению тем нужно сделать флаг типо по 16-й или 18-й строке определяем
#поиск центроида - надо смотреть метоиды для определения темы
#надо выделять всякие прикольные, необычные фичи в кластере
ThemeIdiaOld=function(TN){
  STN = TN #номер темы, которую рассматриваем
  i=1
  im=length(clasters[,1])
  FT=data.frame(NULL) #собираем смысл темы
  length(FT)
  numline=16
  numline1=18
  if(Themeflag == 1)
  {
    numline=18
    numline1=18
  }
  while(i<=im)
  {
    if(clasters[i,1]==STN)
    {
      FT[1,length(FT)+1]=patentlist[numline, as.numeric(Dpatentlist[2,i])]
      FT[2,length(FT)] = patentlist[numline1, as.numeric(Dpatentlist[2,i])]
    }
    i=i+1
  }
  FT[1,]
  pclas <- Corpus(VectorSource(t(FT[1,])))
  pclas <- Corpus(VectorSource(FT[1,]))
  pclas <- tm_map(pclas, tolower) 
  pclas <- tm_map(pclas, removePunctuation)
  pclas <- tm_map(pclas, removeNumbers)
  pclas <- tm_map(pclas, function(x) removeWords(x, stopwords("english")))
  pclas <- tm_map(pclas, function(x) removeWords(x, ObjectDictionary))
  pclas <- tm_map(pclas, function(x) removeWords(x, AdStops))
  pclas <- tm_map(pclas, function(x) removeWords(x, stwords))
  pclas  <- tm_map(pclas, stemDocument, language = "english") 
  length(pclas)
  pclas$content
  ThemeIC=c(findFreqTerms(TermDocumentMatrix(pclas), lowfreq = 0.45*length(unique(pclas))))
  VThemeIC = as.String(paste(ThemeIC[1:length(ThemeIC)]))
  if(length(ThemeIC) < 5)
  {
    ThemeIC=c(findFreqTerms(TermDocumentMatrix(pclas), lowfreq = 0.3*length(unique(pclas))))
    VThemeIC = as.String(paste(ThemeIC[1:length(ThemeIC)]))
  }
  i=1
  im=length(FT[1,])
  SentenceTheme = data.frame(NULL)
  while(i<=im)
  {
    words = strsplit(FT[2,i], '[,|.]')
    words[[1]] = removeNumbers(words[[1]])
    words[[1]] = removePunctuation(words[[1]])
    words[[1]]=removeWords(words[[1]], c('[A-Z]+'))
    j=1
    jm=length(ThemeIC)
    grepvalue = c(NULL)
    while(j<=jm)
    {
      if(length(grep(ThemeIC[j], words[[1]]))>0)
      {
        l=1
        lm=length(grep(ThemeIC[j], words[[1]]))
        while(l<=lm)
        {
          if(length(grepvalue[grepvalue==grep(ThemeIC[j], words[[1]])[l]]) == 0)
            SentenceTheme[1, (length(SentenceTheme)+1)] = paste(words[[1]][grep(ThemeIC[j], words[[1]])[l]])
          l=l+1
        }
        grepvalue=c(grepvalue,grep(ThemeIC[j], words[[1]]))
      }
      j=j+1
    }
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  SentenceTheme = unique(SentenceTheme)
  Tpclas <- Corpus(VectorSource(SentenceTheme[1,])) 
  Tpclas <- tm_map(Tpclas, tolower) 
  Tpclas <- tm_map(Tpclas, removePunctuation)
  Tpclas <- tm_map(Tpclas, removeNumbers)
  Tpclas <- tm_map(Tpclas, function(x) removeWords(x, stopwords("english")))
  Tpclas  <- tm_map(Tpclas, stemDocument, language = "english") 
  #clwords <- TermDocumentMatrix(pclas)
  #clwordsmat = as.matrix(TermDocumentMatrix(pclas))
  Tclwordsmat=unique(as.matrix(TermDocumentMatrix(Tpclas)))
  TlsaSpace= lsa(Tclwordsmat)
  UnderTheme=data.frame(NULL)
  mc=1
  while(mc<=length(TlsaSpace$d[,1]))
  {
    mt=max(abs(TlsaSpace$d[mc,]))
    #mt=max((t(lsaSpace$v)[mc,1:coc]))
    f=1
    while(abs(TlsaSpace$d[mc,f])!=mt)
      #while((t(lsaSpace$v)[mc,f])<mt)
    {
      f=f+1
    }
    UnderTheme[mc,1]=f
    mc=mc+1
    setTxtProgressBar(ProgressBar1, mc/length(TlsaSpace$d[,1])*100)
  }
  Fthemes = data.frame(NULL)
  i=1
  im=max(unique(UnderTheme)[,1])
  while(i<=im)
  {
    j=1
    jm=length(SentenceTheme)
    while(j<=jm)
    {
      if(UnderTheme[j,1] == i)
      {
        Fthemes[1,length(Fthemes)+1] = SentenceTheme[1,j]
        break
      }
      j=j+1
    }
    i=i+1
  }
  return(as.String(Fthemes[1,]))
}
ThemeIdiaOld1=function(TN){
  STN = TN
  i=1
  im=length(HThemeC)
  while(STN>sum(HThemeC[0:i]))
  {
    i=i+1
  }
  STN = STN - sum(HThemeC[0:(i-1)])
  Tmedoid = Medoids[[i]][STN,]
  
  Nmetoid = c(NULL)
  N1 = c(NULL)
  N2 = c(NULL)
  LSAS = LSAlist[[i]]$d
  Themecoords = data.frame(NULL)
  Thcv = 0
  cutclasters = data.frame(NULL)
  cutclastersVol=0
  l=1
  lm=length(clasters[,1])
  while(l<=lm)
  {
    if(clasters[l,1] > sum(HThemeC[0:(i-1)]) && clasters[l,1] <= sum(HThemeC[0:i]))
    {
      cutclastersVol = cutclastersVol+1
      cutclasters[cutclastersVol,1] = clasters[l,1]
      if(i>1)
      {
        cutclasters[cutclastersVol,2] = cutclastersVol
        cutclasters[cutclastersVol,3] = l
      }
      if(i==1)
      {
        cutclasters[cutclastersVol,2] = l
        cutclasters[cutclastersVol,3] = cutclastersVol
      }
    }
    l=l+1
  }
  ji=1
  jim=length(cutclasters[,1])
  while(ji<=jim)
  {
    if(cutclasters[ji,1] == (STN+sum(HThemeC[0:(i-1)])))
    {
      Thcv = Thcv+1
      Themecoords[Thcv,1] = cutclasters[ji,2]#ji
      j=2
      jm=length(LSAS[1,])+1
      while(j<=jm)
      {
        Themecoords[Thcv,j] = LSAS[cutclasters[ji,2],j-1] #lsaSpace$d[ji,j-1] #LSAS[ji-sum(HThemeC[0:(i-1)]),j-1]
        j=j+1
      }
    }
    ji=ji+1
  }
  ij=1
  ijm=length(Themecoords[,1])
  Tmedoid
  while(ij<=ijm)
  {
    TCflag = 0
    if(Tmedoid[1] == Themecoords[ij,2])
    {
      j=3
      jm=length(LSAS[1,])+1
      while(j<=jm)
      {
        if(Tmedoid[j-1] != Themecoords[ij,j])
        {
          TCflag = 0
          break
        }
        TCflag = 1
        j=j+1
      }
    }
    if(TCflag == 1)
    {
      Nmetoid = Themecoords[ij,1]
      break
    }
    ij=ij+1
  }
  ij
  Nmetoid
  Themecoords
  dist.matT = as.matrix(dist(Themecoords[,1:length(LSAS[1,])+1]))
  ij=1
  ijm=length(dist.matT[,1])
  if(length(Themecoords[1,])>1)
  {
    while(ij<=ijm)
    {
      if(Themecoords[ij,1] == Nmetoid)
      {
        DTM = max(dist.matT[ij,])
        j=1
        jm=length(dist.matT[,1])
        while(j<=jm)
        {
          if(dist.matT[ij,j] == DTM)
          {
            N1 = Themecoords[j,1]
            break
          }
          j=j+1
        }
        DTMi = min(dist.matT[ij,][dist.matT[ij,]>0])
        j=1
        jm=length(dist.matT[,1])
        while(j<=jm)
        {
          if(dist.matT[ij,j] == DTMi)
          {
            N2 = Themecoords[j,1]
            break
          }
          j=j+1
        }
      }
      ij=ij+1
    }
  }
  if(i>1)
  {
    Nmetoid = cutclasters[Nmetoid,3]
    N1 = cutclasters[N1,3]
    N2 = cutclasters[N2,3]
  }
  Nmetoid
  N1 
  N2 
  DPLc = 4 
  Idea = data.frame(NULL)
  Idea[1,1] = Dpatentlist[DPLc,Nmetoid]#cutclasters[Nmetoid,2]]
  IdeaVol = 1
  if(length(Themecoords[1,])>1)
  {
    if(Dpatentlist[DPLc,N1]!=Dpatentlist[DPLc,Nmetoid])#cutclasters[Nmetoid,2]])
    {
      IdeaVol=IdeaVol+1
      Idea[IdeaVol,1] = Dpatentlist[DPLc,N1]#cutclasters[N1,2]]
    }
  }
  if(length(Themecoords[1,])>2)
  {
    if(Dpatentlist[DPLc,N2]!=Dpatentlist[DPLc,Nmetoid] && Dpatentlist[DPLc,N1]!=Dpatentlist[DPLc,N2])
    {
      IdeaVol=IdeaVol+1
      Idea[IdeaVol,1] = Dpatentlist[DPLc,N2]
    }
  }
  Idea[,1]
  return(as.String(Idea[,1]))
}
ThemeIdia=function(TN){
  i=1
  im=length(clasters[,1])
  tempvector = c(NULL)
  while(i<=im)
  {
    if(clasters[i,1] == TN)
    {
      tempvector[length(tempvector)+1] = Dpatentlist[1,i]
    }
    i=i+1
  }
  pclas <- Corpus(VectorSource(tempvector))
  pclas <- tm_map(pclas, removeNumbers)
  pclas <- tm_map(pclas, removePunctuation)
  pclas  <- tm_map(pclas, stemDocument, language = "english")
  st = as.matrix(TermDocumentMatrix(pclas))
  stv = data.frame(NULL)
  i=1
  im=length(st[,1])
  while(i<=im)
  {
    stv[i,1] = 'temp'
    stv[i,1] = names(st[,1])[i]
    stv[i,2] = sum(st[i,])
    i=i+1
  }
  stv = stv[order(-stv[,2]),]
  tw = stv[stv[,2]>=sum(stv[stv[,2]>length(tempvector)/5,2])/(1.5*length(stv[stv[,2]>length(tempvector)/5,2])),1]
  tempvector = removePunctuation(tempvector)
  sv = strsplit(tempvector, ' ')
  temptw = tw
  i=1
  im=length(temptw)
  answvector = c(NULL)
  while(i<=im)
  {
    answbreakflag = 0
    ln=grep(temptw[i],sv)
    tw=tw[-1]
    j=1
    jm=length(ln)
    while(j<=jm)
    {
      eln = grep(temptw[i],sv[[ln[j]]][nchar(sv[[ln[j]]])>1])
      o=1
      om=length(eln)
      while(o<=om)
      {
        l=1
        lm=length(temptw)-i
        while(l<=lm)
        {
          ew = grep(tw[l],sv[[ln[j]]][nchar(sv[[ln[j]]])>1])
          m=1
          mm=length(ew)
          while(m<=mm)
          {
            if(ew[m]-eln[o] == -1 || ew[m]-eln[o] == 1)
            {
              answbreakflag=1
              if(ew[m]-eln[o] == -1)
                answvector[length(answvector)+1] = paste0(paste(sv[[ln[j]]][nchar(sv[[ln[j]]])>1][ew[m]], sv[[ln[j]]][nchar(sv[[ln[j]]])>1][eln[o]]),',')
              else
                answvector[length(answvector)+1] = paste0(paste(sv[[ln[j]]][nchar(sv[[ln[j]]])>1][eln[o]], sv[[ln[j]]][nchar(sv[[ln[j]]])>1][ew[m]]),',')
            }
            if(answbreakflag == 1)
              break
            m=m+1
          }
          if(answbreakflag == 1)
            break
          l=l+1
        }
        if(answbreakflag == 1)
          break
        o=o+1
      }
      if(answbreakflag == 1)
        break
      j=j+1
    }
    if(answbreakflag == 0)
    {
      if(i<=im/2)
      answvector[length(answvector)+1] = paste0(sv[[ln[j-1]]][nchar(sv[[ln[j-1]]])>1][eln[o-1]],',')
    }
    i=i+1
  }
  return(as.String(answvector))
}
ClasterGraph=function(TN){
  i=1
  im=length(clasters[,1])
  tempvector = c(NULL)
  tempvc = data.frame(NULL)
  tvcc = 1
  while(i<=im)
  {
    if(clasters[i,1] == TN)
    {
      tempvector[length(tempvector)+1] = Dpatentlist[1,i]
      j=1
      jm=length(PatCoords[1,])
      while(j<=jm)
      {
        tempvc[tvcc,j]=PatCoords[i,j]
        j=j+1
      }
      tvcc=tvcc+1
    }
    i=i+1
  }
  cdm = as.matrix(dist(tempvc, method = "manhattan"))
  fgm = data.frame(NULL)
  i=1
  im=length(tempvc[,1])
  fgmc = 1
  while(i<=im)
  {
    j=i
    jm=im
    while(j<=jm)
    {
      fgm[fgmc,1] = tempvector[i]
      fgm[fgmc,2] = tempvector[j]
      fgm[fgmc,3] = cdm[i,j]
      j=j+1
      fgmc = fgmc+1
    }
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  #далеких уберем из fgm
  i=1
  im=length(fgm[,1])
  while(i<=im)
  {
    if(fgm[i,3] > 0.15 || fgm[i,3] == 0)
    {
      fgm=fgm[-i,]
      im=length(fgm[,1])
    }
    else 
    {
      fgm[i,3] = 1
      i=i+1
    }
  }
  #граф патентов
  actors <- unique(fgm[,1])
  clearC <- data.frame(fgm[,1], fgm[,2])
  g <- graph_from_data_frame(unique(clearC), directed=FALSE, vertices=NULL)
  set.seed(42)
  i <- 1
  n <- length(unique(clearC)[1])
  type <- rep(rgb(as.numeric(t(col2rgb("gold"))[1,1])/255, as.numeric(t(col2rgb("gold"))[1,2])/255, as.numeric(t(col2rgb("gold"))[1,3])/255, alpha=0.5),n)
  alpha <- rep(0.01,n)
  # Выбираем алгоритм для ракладки графа (Kamada-Kawai)
  l3 <- layout.fruchterman.reingold(g)
  # Вершины сети будут в форме круга
  V(g)$shape <- "circle"
  # Задаем цвета вершин и ребер между ними.
  E(g)$color <- type
  E(g)$alpha <- alpha
  i<-1
  length(V(g)$name)
  V(g)$name[2]
  #Раскраска по тематикам
  
  # Уменьшим размеры и толщину стрелок ребер.
  E(g)$arrow.size <- 0
  E(g)$arrow.width <- 0
  # Размер вершины пропорционален ее степени центральности.
  # Берем квадратный корень из степени центральности.
  V(g)$size = 10
  V(g)$label <- V(g)$name
  V(g)$label.cex=1
  V(g)$label.width = 10
  V(g)$alpha <- alpha
  E(g)$width = 1
  # Визуализируем нашу сеть
  plot(g, layout=l3)
}
YearPatentsFull = function(){
  Year=2018
  DpatentlistYear = DpatentlistYearSt(Year)
  #Ypatentgraph = YearPatents(DpatentlistYear)
  Ypatentgraph = YearPatents1(DpatentlistYear)
  Ytemppg = Ypatentgraph
  YMacro = YMacros()
  Yto=c(Ypatentgraph[,2], as.character(YMacro[,2]))
  Yfrom=c(Ypatentgraph[,1], as.character(YMacro[,1]))
  Ypatentgraph = data.frame(From = Yfrom, To = Yto, stringsAsFactors = FALSE)
  Ypatentgraph=unique(Ypatentgraph)
  YConWeight = conweightening()

  
  #работа с отдельными тематиками по годам
  Ythemegraph = Ypthemegraph()
  Ytemptg=data.frame(From = Ythemegraph[,1], To = Ythemegraph[,2], Weigth = Ythemegraph[,3], stringsAsFactors = FALSE)
  YthemegraphM = YTMacros()
  YTMfrom = c(Ythemegraph[,1],as.character(YthemegraphM[,1]))
  YTMto= c(Ythemegraph[,2],YthemegraphM[,2])
  Ythemegraph = unique(data.frame(From = YTMfrom, To = YTMto, stringsAsFactors = FALSE))
  #Ytemptg=Ythemegraph
  #Ythemegraph=unique(Ythemegraph)
  YTConWeight = tconweightening()
  YThemeC = TWeight() #размеры странные у тематик

} # пока никуда не привязана
plotMacrostat = function(N){
  R = N
  #R=3
  MMN = data.frame(NULL)
  i=1
  im=length(SupFrame[,1])
  l=1
  while(i<=im)
  {
    MMN[i,1] = SupFrame[i,1]
    MMN[i,2] = sum(Mtrendsstat[l:(l-1+SupFrame[i,2]),R])
    l=l+SupFrame[i,2]
    i=i+1
  }
  i=1
  im=length(MMN[,1])
  while(i<=im)
  {
    if(MMN[i,2] == 0)
    {
      MMN = MMN[-i,]
      i=i-1
    }
    i=i+1
  }
  MtrendsstatC = Mtrendsstat
  i=1
  im=length(MtrendsstatC[,1])
  while(i<=im)
  {
    if(MtrendsstatC[i,R] == 0)
    {
      MtrendsstatC = MtrendsstatC[-i,]
      i=i-1
      im=length(MtrendsstatC[,1])
    }
    i=i+1
  }
  i=1
  im=length(MtrendsstatC[,1])
  while(i<im)
  {
    j=i+1
    jm=im
    while(j<=jm)
    {
      if(as.character(MtrendsstatC[i,1]) == as.character(MtrendsstatC[j,1]) && as.character(MtrendsstatC[i,2]) == as.character(MtrendsstatC[j,2]))
      {
        MtrendsstatC[i,3] = MtrendsstatC[i,3]+MtrendsstatC[j,3]
        MtrendsstatC[i,4] = MtrendsstatC[i,4]+MtrendsstatC[j,4]
        MtrendsstatC = MtrendsstatC[-(j),]
        im=length(MtrendsstatC[,1])
        jm=im
        j=j-1
      }
      j=j+1
    }
    i=i+1
  }
  df <- data.frame(a = c(MMN[,2], MtrendsstatC[,R]), 
                   b = c(rep('x',length(MMN[,1])), rep('y', length(MtrendsstatC[,1]))), 
                   #c = c(MMN[,1], Mtrendsstat[,2], gl(n=(length(SupFrame[,1])+length(Mtrendsstat[,1])),k=1)),
                   z = unique(gl(n=length(c(MMN[,1], MtrendsstatC[,2])),k=1, length = k*n, labels = c(MMN[,1], MtrendsstatC[,2]))),
                   c1 = c(1:(length(MMN[,1])+length(MtrendsstatC[,1]))))
  labelcoords = c(NULL)
  im=length(SupFrame[,1])
  i=1
  while(i<=im)
  {
      labelcoords[length(labelcoords)+1] = sum(df[i:im,1]) 
    i=i+1
  }
  im=length(MtrendsstatC[,1]) + length(SupFrame[,1])
  i=5
  while(i<=im)
  {
    labelcoords[length(labelcoords)+1] = sum(df[i:im,1]) 
    i=i+1
  }
  #tf <- data.frame(a = labelcoords, b = c(rep('x',length(SupFrame[,1])), rep('y', length(Mtrendsstat[,1]))))
  ggplot(df, aes(x = b, y = a, fill = z, colour=c1), label = c(MMN[,1], MtrendsstatC[,2]))+ 
    #geom_label_repel(aes(b, a, fill=z, label = c(MMN[,1], Mtrendsstat[,2]), color = 'white', box.padding = unit(0.45, "lines"),label.padding = unit(0.08, "lines"))) +
    geom_bar(stat = "identity")+
    geom_text(aes(x=b, y = labelcoords), label = unique(gl(n=length(c(MMN[,1], MtrendsstatC[,2])),k=1, length = k*n, labels = c(MMN[,1], MtrendsstatC[,2]))), stat = "identity")+
    #coord_polar(theta="y")+
    labs(title = "Соотношение влияния макротрендов", x = '', y = '')+
    scale_x_discrete(labels = c('Группы трендов', 'Поисковые слова'))+
    theme_minimal()+
    theme(axis.text.y = element_blank(), plot.background = element_blank(), panel.background = element_blank(), panel.grid = element_blank())+
    guides(fill=FALSE)
}
MSUPS = function(N){
  TiD = N
  #отбираем все патенты группы
  i=1
  im=length(clasters[,1])
  slist = c(NULL)
  while(i<=im)
  {
    if(clasters[i,1] == TiD)
      slist[length(slist)+1] = i
    i=i+1
  }
  i=1
  im=length(DpatentlistYear)
  GPC = c(NULL)
  while(i<=im)
  {
    j=1
    jm=length(slist)
    while(j<=jm)
    {
      if(DpatentlistYear[4,i] == slist[j])
      {
        #GPC[length(GPC)+1] = DpatentlistYear[1,i]
        GPC[length(GPC)+1] = DpatentlistYear[5,i]
      }
      j=j+1
    }
    i=i+1
  }
  #вставка внешних трендов всех уровней
  tr=1
  trm=length(trendsSM)
  MGS = data.frame(c(0), c(0), c(0), stringsAsFactors = FALSE)
  MGSVol = 0
  while(tr<=trm)
  {
    gw=unique(grep(trendsSM[tr],GPC, fixed=TRUE))
    if(is.null(length(gw))) #на тот случай, если пустая строка
      length(gw)=0
    if(length(gw)>=1)
    {
      MGSVol = MGSVol+1
      MGS[MGSVol,1] = MMtrends[tr]
      MGS[MGSVol,2] = SMtrends[tr]
      MGS[MGSVol,3] = length(gw)/length(GPC)
    }
    tr=tr+1
    setTxtProgressBar(ProgressBar1, tr/trm*100)
  }
  return((data.frame(c(MGS[,1]),c(MGS[,2]), c(MGS[,3]),stringsAsFactors = FALSE)))
}
plottrends = function(){
  actors <- unique(TrendsGraph[,1])
  clearC <- TrendsGraph
  g <- graph_from_data_frame(unique(clearC), directed=FALSE, vertices=NULL)
  set.seed(42)
  i <- 1
  n <- length(unique(clearC)[,1])
  type <- rep(rgb(as.numeric(t(col2rgb("gold"))[1,1])/255, as.numeric(t(col2rgb("gold"))[1,2])/255, as.numeric(t(col2rgb("gold"))[1,3])/255, alpha=0.5),n)
  # Выбираем алгоритм для ракладки графа (Kamada-Kawai)
  l3 <- layout.fruchterman.reingold(g)
  # Вершины сети будут в форме круга
  V(g)$shape <- "circle"
  # Задаем цвета вершин и ребер между ними.
  E(g)$color <- type
  # Раскраска по тематикам
  V(g)$color = rep(rgb(.193,.0,.32, alpha = 0.5),length(V(g)))
  V(g)$name
  trendscol = c(NULL)
  i=1
  im=length(unique(Mtrends))
  while(i<=im)
  {
    trendscol = c(trendscol, rep(i, length(grep(Mtrends[i], Mtrends))))
    i=i+1
  }
  i=1
  im=length(unique(TrendsGraph[,1]))
  while(i<=im)
  {
    j=1
    jm=length(trends)
    while(j<=jm)
    {
      if(V(g)$name[i] == as.character(trends[j]))
      {
        V(g)$color[i] = rgb(as.numeric(t(col2rgb(as.numeric(trendscol[j])))[1,1])/255, as.numeric(t(col2rgb(as.numeric(trendscol[j])))[1,2])/255, as.numeric(t(col2rgb(as.numeric(trendscol[j])))[1,3])/255, alpha=0.5)
        break
      }
      j=j+1
    }
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  # Уменьшим размеры и толщину стрелок ребер.
  E(g)$arrow.size <- 0
  E(g)$arrow.width <- 0
  # # Размер вершины пропорционален размеру тренда
  i=1
  im=length(unique(TrendsGraph[,1]))
  while(i<=im)
  {
    j=1
    jm=length(USMtrends)
    while(j<=jm)
    {
      if(V(g)$name[i] == as.character(trendsweight[j,1]))
      {
        V(g)$size[i] = trendsweight[j,2]^(1/3)
        break
      }
      j=j+1
    }
    i=i+1  
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  V(g)$label <- V(g)$name
  E(g)$lty=rep(1,length(E(g)))                            # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
  E(g)$curved=rep(0,length(E(g))) 
  E(g)$width=1
  i=1
  im=length(TrendsCon)
  while(i<=im)
  {
    E(g)$width[i]=TrendsCon[i]*5
    if(E(g)$width[i] == 0)
      E(g)$width[i] = 0.000001
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  if(trendsgraphflag == 1)
  {
    i=1
    im=length(Gtrends[,1])
    while(i<=im)
    {
      V(g)[V(g)$name == as.character(Gtrends[i,1])]$shape = "square"
      i=i+1
    }
  }
  # Визуализируем нашу сеть
  if(trendsgraphflag == 0)
  {
    plot(g, layout=l3)
  }
  if(trendsgraphflag == 1)
  {
    MarkList = list(NULL)
    MarkVol = 0
    i=1
    im=length(NewMGroups)
    while(i<=im)
    {
      MarkVol = MarkVol +1
      MarkList[[MarkVol]] = c(NULL)
      j=1
      jm=length(NewMGroups[[i]])
      while(j<=jm)
      {
        o=1
        om = length(V(g))
        while(o<=om)
        {
          if(V(g)$name[o] == as.character(NewMGroups[[i]][j]))
          {
            if(j==1)
              MarkList[[MarkVol]] = o
            if(j>1)
              MarkList[[MarkVol]][j] = o
          }
          o=o+1
        }
        j=j+1
      }
      i=i+1
    }
    AN = MarkVol
    #выделить группы
    A = rgb(as.numeric(t(col2rgb("blue"))[1,1])/255, as.numeric(t(col2rgb("blue"))[1,2])/255, as.numeric(t(col2rgb("blue"))[1,3])/255, alpha=0.2)
    B = rgb(as.numeric(t(col2rgb("green"))[1,1])/255, as.numeric(t(col2rgb("green"))[1,2])/255, as.numeric(t(col2rgb("green"))[1,3])/255, alpha=0.2)
    plot(g, layout=l3,mark.groups=MarkList, mark.col=c(rep(A,AN)), mark.border=NA)
  }
}
detectTW = function() {
  FtrendsW = c(NULL)
  k=0
  q=1
  qm=length(USMtrends)
  while(q<=qm)
  {
    lm = NSMtrends[q]
    l=1
    WMS = 0
    while(l<=lm)
    {
      k=k+1
      WMS = WMS + length(grep(trendsSM[k], DpatentlistYear[5,]))
      l=l+1
    }
    FtrendsW[length(FtrendsW)+1] = WMS
    q=q+1
    setTxtProgressBar(ProgressBar1, q/qm*100)
  }
  return(FtrendsW)
}
detectTC = function(){
  FtrendsCor = data.frame(NULL)
  trendsCorVol = 0
  q = 1
  qm = length(USMtrends)
  k = 1
  tclength = length(lsaSpace$d[1,])
  while(q<=qm)
  {
    j=1
    jm = NSMtrends[q]
    plist = c(NULL)
    while(j<=jm)
    {
      plist = c(plist, grep(trendsSM[k], DpatentlistYear[5,]))
      j=j+1
      k=k+1
    }
    l=1
    lm = length(plist)
    tcoords = c(rep(0,tclength))
    while(l<=lm)
    {
      tcoords = tcoords + lsaSpace$d[DpatentlistYear[4,plist[l]],]
      l=l+1
    }
    m=1
    mm=length(tcoords)
    trendsCorVol = trendsCorVol+1
    tcoords = tcoords / lm
    while(m<=mm)
    {
      FtrendsCor[trendsCorVol,m] = tcoords[m]
      m=m+1
    }
    q=q+1
    setTxtProgressBar(ProgressBar1, q/qm*100)
  }
  return(FtrendsCor)
}
detectTG = function(){
  FTrendsGraph = data.frame(NULL)
  FTrendsGraphVol = 0
  FTrendsCon = c(NULL)
  q=1
  qm=length(USMtrends)
  while(q<=qm)
  {
    j=1
    jm=qm
    Trflag = 0
    while(j<=jm)
    {
      if(j>q)
      {
        if(trdist.mat[q,j] > 0 && trdist.mat[q,j] <= ConDistT)
        {
          FTrendsGraphVol = FTrendsGraphVol + 1
          FTrendsGraph[FTrendsGraphVol, 1] = USMtrends[q]
          FTrendsGraph[FTrendsGraphVol, 2] = USMtrends[j]
          FTrendsCon[length(FTrendsCon)+1] = (ConDistT - trdist.mat[q,j])/(ConDistT)
          Trflag = 1
        }
      }
      j=j+1
    }
    if(Trflag == 0 && sum(trdist.mat[q,]) > 0)
    {
      FTrendsGraphVol = FTrendsGraphVol + 1
      FTrendsGraph[FTrendsGraphVol, 1] = USMtrends[q]
      FTrendsGraph[FTrendsGraphVol, 2] = USMtrends[q]
      FTrendsCon[length(FTrendsCon)+1] = 0
    }
    q=q+1
  }
  return(data.frame(c(FTrendsGraph[,1]),c(FTrendsGraph[,2]), c(FTrendsCon), stringsAsFactors = FALSE))
}
#анализ связи отдельных групп с трендами
SBT = function(N){
  #1) отбираем все патентны группы
  PG = N
  grouppatents = c(NULL)
  i=1
  im=length(clasters[,1])
  while(i<=im)
  {
    if(clasters[i,1] == PG)
      grouppatents[length(grouppatents)+1] = i
    i=i+1
  }
  GPDF = data.frame(NULL)
  GPDFVol = 0
  i=1
  im=length(DpatentlistYear[1,])
  while(i<=im)
  {
    j=1
    jm=length(grouppatents)
    while(j<=jm)
    {
      if(DpatentlistYear[4,i] == grouppatents[j])
      {
        GPDFVol=GPDFVol+1
        GPDF[1,GPDFVol] = DpatentlistYear[1,i]
        GPDF[2,GPDFVol] = DpatentlistYear[5,i]
      }
      j=j+1
    }
    i=i+1
  }
  #2) смотрим структуру поддержки макротрендами
  i=1
  im=length(trendsfu[,1])
  j=1
  k = length(trendsfu[trendsfu[,1] == USMtrends[j],1])
  DpYS = stemDocument(as.character(GPDF[2,]), language = 'english')
  Gprofile = c(NULL)
  while(i<=im)
  {
    gw = length(grep(trendsfu[i,2], DpYS))
    gw[is.na(gw)] = 0
    Gplength = length(Gprofile[j])
    Gplength[is.na(Gprofile[j])] = 0
    if(Gplength>0)
    {
      Gprofile[j] = Gprofile[j]+gw
    }
    if(Gplength==0)
    {
      Gprofile[j] = gw
    }
    if(i==k)
    {
      j=j+1
      k=k+length(trendsfu[trendsfu[,1] == USMtrends[j],1])
    }
    i=i+1
  }
  return(Gprofile)
}
plotGroupDependence = function(){
  if(length(SupByTrends[SupByTrends>0])>0)
  {
    df <- data.frame(a = c(SupByTrends[SupByTrends>0]), 
                   b = c(rep('x',length(USMtrends[SupByTrends>0]))), 
                   #c = c(MMN[,1], Mtrendsstat[,2], gl(n=(length(SupFrame[,1])+length(Mtrendsstat[,1])),k=1)),
                   #z = c(USMtrends[SupByTrends1>0]))
                   z = unique(gl(n=length(SupByTrends[SupByTrends>0]),k=1, length = k*n, labels = c(USMtrends[SupByTrends>0]))))
    labelcoords = c(NULL)
    im=length(USMtrends[SupByTrends>0])
    i=1
    while(i<=im)
    {
      labelcoords[length(labelcoords)+1] = sum(df[i:im,1]) 
      i=i+1
    }
    ggplot(df, aes(x = b, y = a, fill = z), label = c(USMtrends[SupByTrends>0]))+ 
      geom_bar(stat = "identity")+
      geom_text(aes(x=b, y = labelcoords), label = c(USMtrends[SupByTrends>0]), stat = "identity")+
      #coord_polar(theta="y")+
      labs(title = "Влияние трендов на тему", x = '', y = '')+
      scale_x_discrete(labels = c('Тренды влияния и их соотношение'))+
      theme_minimal()+
      theme(axis.text.y = element_blank(), plot.background = element_blank(), panel.background = element_blank(), panel.grid = element_blank())+
      guides(fill=FALSE)
  }
}
SBTD = function(N){
  i=1
  im=length(trendsfu[,1])
  j=1
  k = length(trendsfu[trendsfu[,1] == USMtrends[j],1])
  DpYS = stemDocument(as.character(Dpatentlist[5,N]), language = 'english')
  Gprofile = c(NULL)
  while(i<=im)
  {
    gw = length(grep(trendsfu[i,2], DpYS))
    gw[is.na(gw)] = 0
    Gplength = length(Gprofile[j])
    Gplength[is.na(Gprofile[j])] = 0
    if(Gplength>0)
    {
      Gprofile[j] = Gprofile[j]+gw
    }
    if(Gplength==0)
    {
      Gprofile[j] = gw
    }
    if(i==k)
    {
      j=j+1
      k=k+length(trendsfu[trendsfu[,1] == USMtrends[j],1])
    }
    i=i+1
  }
  return(Gprofile)
}
#####Тело программы####

#devtools::install_github("johndharrison/binman")
#devtools::install_github("johndharrison/wdman")
#devtools::install_github("ropensci/RSelenium")

#механическая кластеризация
#НЕОБХОДИМО ФОРМИРОВАНИЕ СПИСКА КЛЮЧЕВЫХ СЛОВ
#НЕОБХОДИМА НАСТРОЙКА ГИБКОСТИ

#масштабируемая макрокластеризация
Macrotrends = c('safety', 'risk', 'trouble', 'securing', 'environment', 'cleaning', 'ecology', 'emission') #тенденции макроуровня: экология, безопасность
Macrotrendslength=length(Macrotrends)
Megatrends = c('castomization', 'digital', 'technology', 'IT', 'IoT', 'telecommunication', 'optimization', 'business', 'economy', 'efficiency', 'ecosystem', 'standart', 'systematization', 'classification', 'reglamentation') #производственные тенденции: кастомизация, бизнес-оптимизация, оптимизация процессов
Megatrendslength=length(Megatrends)
Mezotrends = c('data', 'internet', 'wireless', 'broadband', 'quantum', 'distributed', 'energy', 'materials', 'robot', 'neuro', 'biology')
Mezotrendslength=length(Mezotrends)
Usagetrends = c('army', 'education', 'entertainment', 'construction', 'medicine', 'transport', 'health', 'manufacture', 'engineering')
Usagetrendslength=length(Usagetrends)
trends=c(Macrotrends, Megatrends,Mezotrends,Usagetrends) 
Mtrends=c(rep('Macrotrends', Macrotrendslength), rep('Megatrends', Megatrendslength),rep('Mezotrends', Mezotrendslength), rep('Usagetrends',Usagetrendslength))


#глобальные объекты
ProgressBar = txtProgressBar(min=0, max=100, style=3)
ProgressBar1 = txtProgressBar(min=0, max=100, style=3)
patentlist = data.frame(NULL)
TechTermDictionary = data.frame(NULL)
DictVolume=0 #объем словарая
ObjectDictionary = c("invention", "model", "method", "mechanism", "program", "system", "disclosure", "apparatus", "device", "vehicle", "drone")
Themeflag = 0 #если 1 то определяет наиболее часто повторяемые слова по 18-й строке патентлиста (полное содержание абстракта)
ThemeGraphFlag = 0 #флаг, регулирующий отображать ли растущие темы квадратиками и треугольниками
YearAnalysisFlag = 1 #если флаг равен единице, то рассматриваются детально два года, а не три
trendsgraphflag = 0 #1 - тогда с выделением растущих и групп сближающихся на графике
ConDist = 1.5 #0.15 #уровень дистанции для связи патентов
ConDistT = 0.5 #0.1 #уровень дистанции для связи трендов
#CLevel = 10 #уровень дистанции для определения количества кластеров - дополнительно меняется в коде
#BSublevel = CLevel-3 #определение количества субкластеров - дополнительно меняется в коде
BRGTflag = 0 #если 1, то нет тенденций к заполнению белых пятен и фундаментальных барьеров (выставляется автоматически)
Detailing = 1 #если 4, то по объединению использований и названий

#request examples
#request = "(Virtual OR Augmented OR Merged OR Extended) NEAR Reality"
request = '(blockchain OR (distributed NEAR registry))'

Sys.setlocale(,"en_EN")

###PARCING OR READY DATA###

#patentlist is input - result of parcing OR just imported data

#ищем и обрезаем лишнее
grep(patentlist[1,1], patentlist[1,])
grep(patentlist[1,1], patentlist[1,])[2]-1
patentlist=patentlist[,1:grep(patentlist[1,1], patentlist[1,])[2]-1]
#patentlist=patentlist[,2:length(patentlist)]



#####Работаем с патентлистом#####
#вытаскиваем даты - альтернативный алгоритм 
el = length(patentlist)
d = 1
while(d<=el)
{
  Pdata = c(NULL)
  grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1]
  if(is.na(grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1]))
  {
    length(Pdata)=0
    Pdata = 0
  }
  if(is.na(grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1]) == FALSE)
  {
    if(length(grep('.[0-9][0-9][0-9][0-9]$', patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d]))>0)
      Pdata = substr(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d], nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d])-3, nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d]),d]))
    else
      Pdata = substr(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d], nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d])-3-nchar(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d], '.[0-9][0-9][0-9][0-9]')[[1]][length(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d], '.[0-9][0-9][0-9][0-9]')[[1]])]), nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d]),d])-nchar(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d], '.[0-9][0-9][0-9][0-9]')[[1]][length(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d], '.[0-9][0-9][0-9][0-9]')[[1]])]))
  }
  if(length(Pdata)>0)
  {
    patentlist[2,d] = Pdata
    #patentlist[2,1]
    #nchar(patentlist[2,1])
  }
  if(length(Pdata)==0)
  {
    if(d!=el && d!=1)
    {
      if(is.na(grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1]))
         TPdata = 0
      if(is.na(grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1])==FALSE)
        if(length(grep('.[0-9][0-9][0-9][0-9]$', patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d]))>0)
          Pdata = substr(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1], nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1])-3, nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1]),d-1]))
        else
          Pdata = substr(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1], nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1])-3-nchar(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1], '.[0-9][0-9][0-9][0-9]')[[1]][length(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1], '.[0-9][0-9][0-9][0-9]')[[1]])]), nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1]),d-1])-nchar(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1], '.[0-9][0-9][0-9][0-9]')[[1]][length(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d-1])[1],d-1], '.[0-9][0-9][0-9][0-9]')[[1]])]))
      if(is.na(grep('.[0-9][0-9][0-9][0-9]()?', patentlist[,d+1])[1]))
        TNdata = 0
      if(is.na(grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1])==FALSE)
        if(length(grep('.[0-9][0-9][0-9][0-9]$', patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d])[1],d]))>0)
          Pdata = substr(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1], nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1])-3, nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1]),d+1]))
        else
          Pdata = substr(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1], nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1])-3-nchar(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1], '.[0-9][0-9][0-9][0-9]')[[1]][length(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1], '.[0-9][0-9][0-9][0-9]')[[1]])]), nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1]),d+1])-nchar(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1], '.[0-9][0-9][0-9][0-9]')[[1]][length(strsplit(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1], '.[0-9][0-9][0-9][0-9]')[[1]])]))
      if(length(TPdata)>0 && length(TNdata)>0)
      {
        if(TPdata==TNdata)
          patentlist[2,d] = TPdata
        if(TPdata!=TNdata)
          patentlist[2,d] = TNdata
      }
      else
      {
        patentlist[2,d] = 0
      }
    }
    if(d==el)
    {
      break
    }
    if(d==1)
    {
      TNdata = substr(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1], nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?', patentlist[,d+1])[1],d+1])-13, nchar(patentlist[grep('.[0-9][0-9][0-9][0-9]( )?$', patentlist[,d+1]),d+1])-10)
      if(length(TNdata)>0)
      {
        patentlist[2,d] = TNdata
      }
      else
      {
        patentlist[2,d] = 0
      }
    }
  }
  d = d+1
  setTxtProgressBar(ProgressBar, d/el*100)
}

#удалить текст запроса, если он встречается в патенте (может не оказывать влияние на результат, но может быть и обратное в ряде случаев)
i=1
im=length(patentlist)
while(i<=im)
{
  patentlist[grep(removePunctuation(SRequest), removePunctuation(patentlist[,i])),i] = ""
  i=i+1
  setTxtProgressBar(ProgressBar, i/im*100)
}

#объединить все последующие ячейки в одну для корпуса
patentlist[is.na(patentlist)] <- " "
colpat = length(patentlist)
nad=1
while(nad<=colpat)
{
  pol=4
  while(pol<=length(patentlist[,nad]))
  {
    if(nchar(patentlist[pol,nad])>3)
    {
      patentlist[3,nad]=paste(patentlist[3,nad], patentlist[pol,nad])
    }
    pol = pol+1
  }
  nad=nad+1
  setTxtProgressBar(ProgressBar, nad/colpat*100)
}

#обрезаем малополезное
nad=1
colpat = length(patentlist)
while(nad<=colpat)
{
  cutl = nchar(strsplit(patentlist[3,nad], '[A-Za-z0-9,/ ]+[a-z][A-Z]')[[1]][2:length(strsplit(patentlist[3,nad], '[A-Za-z0-9,/ ]+[a-z][A-Z]')[[1]])])
  patentlist[4,nad] = substr(patentlist[3,nad], nchar(patentlist[3,nad])-cutl, nchar(patentlist[3,nad]))
  nad=nad+1
  setTxtProgressBar(ProgressBar, nad/colpat*100)
}

#FastText model
PreVec = c(NULL)
i=1
im=length(patentlist[1,])
while(i<=im)
{
  PreVec[length(PreVec)+1] = patentlist[4,i]
  i=i+1
}
PatContVector = removeWords(removeNumbers(tolower(removePunctuation(PreVec))), stopwords(kind = 'en'))
i=1
im=length(PatContVector)
while(i<=im)
{
  while(length(grep("  ", PatContVector[i]))>0)
    PatContVector[i] = gsub('  ', ' ', PatContVector[i])
  if(length(grep("^ ", PatContVector[i]))>0)
    PatContVector[i] = gsub("^ ", "", PatContVector[i])
  i=i+1
}
i=1
im=length(PatContVector)
while(i<=im)
{
  if(is.na(PatContVector[i]))
  {
    PatContVector=PatContVector[-i]
    im=im-1
  }
  else
   i=i+1
}
i=1
im=length(PatContVector)
while(i<=im)
{
  if(nchar(PatContVector[i]) <= 5)
  {
    PatContVector=PatContVector[-i]
    im=im-1
  }
  else
    i=i+1
}

#ТУТ СДЕЛАЛИ МОДЕЛЬ, дальше ее можно лишь дополнять
oldf = read.csv(file = "PatContVector.txt", stringsAsFactors = FALSE)
oldf=oldf[,1]
PatContVector = c(PatContVector, oldf)
fileConn<-file("PatContVector.txt")
writeLines(PatContVector, fileConn)
close(fileConn)

FTM = fasttext('PatContVector.txt', method = "skipgram", control = ft.control())


PCTflag = 0 #если 1 то вдобавок РСТ заявки
LowYear = 2007
patentlistt=patentlist
patentlist=data.frame(NULL)
PV=0
nad=1
colpat=length(patentlistt)
while(nad<=length(patentlistt))
{
  if(is.na(length(patentlistt[2,nad])))
    length(patentlistt[2,nad]) = 0
  if(nchar(patentlistt[2,nad]) > 1)
  {
    if(PCTflag==0)
      if(as.numeric(patentlistt[2,nad])>=LowYear && length(patentlistt[2,nad]) > 0)
      {
        PV=PV+1
        i=1
        while(i<=19)
        {
          patentlist[i,PV]=patentlistt[i,nad]
          i=i+1
        }
      }
    if(PCTflag==1)
      if(as.numeric(patentlistt[2,nad])>=LowYear && patentlistt[4,nad] == 'WO' && length(patentlistt[2,nad]) > 0)
      {
        PV=PV+1
        i=1
        while(i<=19)
        {
          patentlist[i,PV]=patentlistt[i,nad]
          i=i+1
        }
      }
  }
  nad=nad+1
  setTxtProgressBar(ProgressBar, nad/colpat*100)
}
patentlistt[nad,2]

#выделение сложных форм (масштабируемый алгоритм) 
TextCol = 3
AimCol = 5
stwords = c('^[F|f]or$','^[R|r]efers$','^[R|r]elates$', '^[T|t]o$', '^[B|b]e$')
ingwords = c('ing$', 'ity$', 'ment$', 'ion$', 'ate$') #два слова после - если st и ing стоят подряд, если между ними слово - то его (максимум два слова между ними) + два слова после
colpat = length(patentlist)
nad=1
while(nad<=colpat)
{
  words = strsplit(patentlist[TextCol,nad], ' ')
  words[[1]]=removePunctuation(words[[1]])
  words[[1]]=removeNumbers(words[[1]])
  words[[1]]=removeWords(words[[1]], c('^ing$', '^ity$','^ying$', '^sion$', '^ding$', '^king$')) #удаляем стоп-слова
  sti = 1
  while(sti <= length(stwords)) #поиск по stwords
  {
    st = grep(stwords[sti], words[[1]])
    cut=0
    st[is.na(st)] <- 0
    if(length(st)==0 || is.null(st))
    {
      st=0
    }
    ingi = 1
    stl = st
    while(ingi<=length(ingwords))
    {
      inc=1
      while(inc<=length(stl))
      {
        st=stl[inc]
        if(st>0)
        {
          ing = grep(ingwords[ingi],words[[1]])
          ing
          ing[is.na(ing)] <- 0
          if(length(ing)==0 || is.null(ing))
          {
            ing=0
          }
          ings=1
          while(ings <= length(ing))
          {
            if(ing[ings]>st)
            {
              ing = ing[ings]
              break
            }
            ings=ings+1
          }
          if(length(ing)>1)
          {
            ing=0
          }
          cut = ing - st
          if(cut<0)
          {
            cut=0
          }
          if(cut>5)
          {
            cut=0
          }
          #включение слова в словарь
          if(st > 0 && cut > 0)
          {
            DictVolume=DictVolume+1
            TechTermDictionary[DictVolume,1]=words[[1]][ing]
            TechTermDictionary[DictVolume,2]=1
            intgr = 1
            while(intgr <= cut+2)
            {
              ncw = nchar(words[[1]][intgr+st])
              if(is.na(ncw))
              {
                break
              }
              if(nchar(words[[1]][intgr+st])<4) #пропуск трехбуквенных слов в счетчике
              {
                cut=cut+1
              }
              TechTermDictionary[DictVolume,2] = paste(TechTermDictionary[DictVolume,2], words[[1]][intgr+st])
              intgr=intgr+1
            }
          }
        }
        if(st>0 && cut>0 && length(grep(words[[1]][ing], patentlist[16,nad]))==0)
        {
          if(length(grep('[a-z]',patentlist[AimCol,nad]))>0)
          {
            patentlist[AimCol,nad] = paste(patentlist[AimCol,nad],",")
          }
          intgr = 1
          while(intgr <= cut+2)
          {
            ncw = nchar(words[[1]][intgr+st])
            if(is.na(ncw))
            {
              break
            }
            if(nchar(words[[1]][intgr+st])<4) #пропуск трехбуквенных слов в счетчике
            {
              cut=cut+1
            }
            patentlist[AimCol,nad] = paste(patentlist[AimCol,nad], words[[1]][intgr+st])
            intgr=intgr+1
          }
        }
        inc=inc+1
      }
      ingi=ingi+1
    }
    sti=sti+1
  }
  od = 1
  while(od <= length(ObjectDictionary)) #поиск по ObjectDictionary
  {
    st = grep(ObjectDictionary[od], words[[1]])
    cut=0
    st[is.na(st)] <- 0
    if(length(st)==0 || is.null(st))
    {
      st=0
    }
    ingi = 1
    stl = st
    while(ingi<=length(ingwords))
    {
      inc=1
      while(inc<=length(stl))
      {
        st=stl[inc]
        if(st>0)
        {
          ing = grep(ingwords[ingi],words[[1]])
          ing[is.na(ing)] <- 0
          if(length(ing)==0 || is.null(ing))
          {
            ing=0
          }
          ings=1
          while(ings <= length(ing))
          {
            if(ing[ings]>st)
            {
              ing = ing[ings]
              break
            }
            ings=ings+1
          }
          if(length(ing)>1)
          {
            ing=0
          }
          cut = ing - st
          if(cut<0)
          {
            cut=0
          }
          if(cut>5)
          {
            cut=0
          }
          #включение слова в словарь
          if(st > 0 && cut > 0)
          {
            DictVolume=DictVolume+1
            TechTermDictionary[DictVolume,1]=words[[1]][ing]
            TechTermDictionary[DictVolume,2]=1
            intgr = 1
            while(intgr <= cut+2)
            {
              ncw = nchar(words[[1]][intgr+st])
              if(is.na(ncw))
              {
                break
              }
              if(nchar(words[[1]][intgr+st])<4) #пропуск трехбуквенных слов в счетчике
              {
                cut=cut+1
              }
              TechTermDictionary[DictVolume,2] = paste(TechTermDictionary[DictVolume,2], words[[1]][intgr+st])
              intgr=intgr+1
            }
          }
          if(st>0 && cut>0 && length(grep(words[[1]][ing], patentlist[AimCol,nad]))==0)
          {
            if(length(grep('[a-z]',patentlist[AimCol,nad]))>0)
            {
              patentlist[AimCol,nad] = paste(patentlist[AimCol,nad],",")
            }
            intgr = 1
            while(intgr <= cut+2)
            {
              ncw = nchar(words[[1]][intgr+st])
              if(is.na(ncw))
              {
                break
              }
              if(nchar(words[[1]][intgr+st])<4) #пропуск трехбуквенных слов в счетчике
              {
                cut=cut+1
              }
              patentlist[AimCol,nad] = paste(patentlist[AimCol,nad], words[[1]][intgr+st])
              intgr=intgr+1
            }
          }
        }
        inc=inc+1
      }
      ingi=ingi+1
    }
    od=od+1
  }
  nad=nad+1
  setTxtProgressBar(ProgressBar, nad/colpat*100)
  #print(nad)
  #flush.console()
}
close(ProgressBar)

#включение названия и определенных применений в 10-ю строку
NameAimCol = 6
patentlist[NameAimCol,]=''
nad=1
nadm = length(patentlist)
while(nad<=nadm)
{
  patentlist[NameAimCol,nad] = paste(removeWords(removeNumbers(removePunctuation(patentlist[1,nad])), 'WO'), ',', patentlist[AimCol,nad])
  nad=nad+1
  setTxtProgressBar(ProgressBar, nad/nadm*100)
}

AdStops = c('one','two','three', 'use', 'used', 'can', 'may', 'high', 'low', 'via', 'also', 'the', 'a', 'it', 'as', 'for', 'even', 'and', 'in', 'an', 'of')
#AdStops=c(AdStops,findFreqTerms(TermDocumentMatrix(pclas), lowfreq = 0.02*length(pclas)))
AdStops=unique(AdStops)

#убираем часто повтояремые слова
i=1
im=length(patentlist)
while(i<=im)
{
  patentlist[AimCol,i] = tolower(patentlist[AimCol,i])
  patentlist[AimCol,i] = removeWords(patentlist[AimCol,i], stopwords(kind='en'))
  patentlist[AimCol,i] = removeWords(patentlist[AimCol,i], AdStops)
  i=i+1
  setTxtProgressBar(ProgressBar, i/im*100)
}

#определение % определенных использований
counter = 0
colpat = length(patentlist)
nad=1
AimCol
while(nad<=colpat)
{
  if(length(grep('[a-z]', patentlist[AimCol,nad]))>0)
    counter = counter +1
  nad = nad+1
}
share = counter / nad
share


#####Работаем с Dпатентлистом#####
#исключаем неопределенные патенты
Dpatentlist=data.frame(NULL)
DpatentlistVol=0
nad=1
colpat=length(patentlist)
while(nad<=length(patentlist))
{
  if(is.na(nchar(patentlist[AimCol,nad])))
    break
  if(is.null(nchar(patentlist[AimCol,nad])))
    break
  if(nchar(patentlist[AimCol,nad])>1)
  {
    DpatentlistVol=DpatentlistVol+1
    Dpatentlist[1,DpatentlistVol]=patentlist[AimCol,nad]
    Dpatentlist[2,DpatentlistVol]=nad
    Dpatentlist[3,DpatentlistVol]=patentlist[2,nad]
    Dpatentlist[4,DpatentlistVol]=patentlist[NameAimCol,nad]
    Dpatentlist[5,DpatentlistVol]=patentlist[TextCol,nad]
  }
  nad=nad+1
  setTxtProgressBar(ProgressBar, nad/colpat*100)
}

#кластеризация на основе предобученной FastText
#сначала уберем дублирования в вырезанных кусках - такой проблемы то и нет
#подбор координат для каждого документа
Detailing = 1
PatCoords = data.frame(NULL)
i=1
im=length(Dpatentlist[1,])
while(i<=im)
{
  A = tolower(removePunctuation(Dpatentlist[1,i]))
  while(length(grep("  ", A))>0)
    A = gsub('  ', ' ', A)
  if(length(grep("^ ", A))>0)
    A = gsub("^ ", "", A)
  j=1
  jm=length(c(as.matrix(get_word_vectors(FTM, A))))
  while(j<=jm)
  {
    PatCoords[i,j] = c(as.matrix(get_word_vectors(FTM, A)))[j]
    j=j+1
  }
  i=i+1
  setTxtProgressBar(ProgressBar, i/im*100)
}


dist.mat = as.matrix(dist(PatCoords, method = "manhattan"))

WMT = get_word_vectors(FTM, get_words(FTM))
dist.matM = as.matrix(dist(WMT, method = "manhattan"))
dist.matM = as.data.frame(dist.matM)

trendss <- stemDocument(trends, language = "english")
wordslist = names(dist.matM)
trendsSM = c(NULL)
SMtrends = c(NULL)
MMtrends = c(NULL)
testcounter = 0
j=1
jm=length(trendss)
while(j<=jm) #надо подобрать другую границу дистанции - дистанция завивист от количества документов
{
  RS = dist.matM[[as.String(trendss[j])]]
  RL = c(NULL)
  if(length(RS)>0)
  {
    i=1
    im=length(dist.matM)
    while(i<=im)
    {
      if(as.numeric(RS[i])<=0.005)
      {
        RL[length(RL)+1] = i
        testcounter = testcounter+1
      }
      i=i+1
    }
    o=1
    om=length(RL)
    while(o<=om)
    {
      trendsSM[length(trendsSM)+1] = wordslist[RL[o]]
      SMtrends[length(SMtrends)+1] = trends[j]
      MMtrends[length(MMtrends)+1] = Mtrends[j]
      o=o+1
    }
  }
  j=j+1
  setTxtProgressBar(ProgressBar, j/jm*100)
}
testcounter
#ручное дополнение трендов
SMA = c('medicine','medicine')
ASM = c('health', 'healthcare')
SMN = c(NULL)
NSM = c(NULL)
NMMtrends = c(NULL)
i=1
j=1
im=length(SMtrends)
while(i<=im)
{
  j=1
  jm=length(SMA)
  while(j<=jm)
  {
    if(SMtrends[i] == SMA[j])
    {
      SMN[length(SMN)+1] = SMA[j]
      NSM[length(NSM)+1] = stemDocument(ASM[j], language = 'english')
      NMMtrends[length(NMMtrends)+1] = MMtrends[i]
      SMA = SMA[-j]
      ASM = ASM[-j]
      j=j-1
      jm=length(SMA)
    }
    j=j+1
  }
  SMN[length(SMN)+1] = SMtrends[i]
  NSM[length(NSM)+1] = trendsSM[i]
  NMMtrends[length(NMMtrends)+1] = MMtrends[i]
  i=i+1
}
MMtrends = NMMtrends
SMtrends = SMN
trendsSM = NSM

#ТИПО АЛГОРИТМ ДЛЯ ОПРЕДЕЛЕНИЯ КОЛИЧЕСТВА КЛАСТЕРОВ - надо настраивать в зависимости от выборки
CLevel = sum(dist.mat)/length(dist.mat) * 2.1
i=1
im=length(dist.mat[,1])
KCounter = 0
while(i<=im)
{
  if((sum(dist.mat[i,])/length(dist.mat[1,]))>=CLevel)
    KCounter = KCounter +1
  i=i+1
}
KCounter

#кластеризация на основе Clara (лучше по lsaSpace$s)
#object = clara(lsaSpace$d, k=length(lsaSpace$s))
object = clara(PatCoords, k=KCounter, metric = "manhattan", sampsize = length(PatCoords[,1]), samples = length(PatCoords[,1])*2) #чем меньше кластеров, тем большая изначально дистанция рассматривается как "эталон", сэмпсайз позволяет изменить это
#object = clara(lsaSpace$d, k=KCounter, metric = "manhattan", sampsize = length(lsaSpace$d[,1]), samples = length(lsaSpace$d[,1])*2) #чем меньше кластеров, тем большая изначально дистанция рассматривается как "эталон", сэмпсайз позволяет изменить это
#object = clara(lsaSpace$d, k=KCounter, metric = "manhattan", sampsize = 500, samples = 100) #чем меньше кластеров, тем большая изначально дистанция рассматривается как "эталон", сэмпсайз позволяет изменить это
clasters = as.data.frame(object$clustering)
#KCounter = 123
#object$medoids
#length(lsaSpace$s)

#те кластеры что слишком большие разложить на субкластеры (фундаментальные барьеры искать в этих субкластерах)

#определение веса тематик
ThemeC=data.frame(NULL)
tn=1
while(tn<=KCounter)
{
  nad=1
  length(clasters)
  tcount=0
  while(nad<=length(clasters[,1]))
  {
    if(clasters[nad,1]==tn)
    {
      tcount=tcount+1
    }
    nad=nad+1
  }
  ThemeC[tn,1] = tcount
  tn=tn+1
  setTxtProgressBar(ProgressBar1, tn/KCounter*100)
}
sum(ThemeC[,1])

#определение больших кластеров - создание перечня документов, входивших в эти кластеры - дополнение более мелкими кластерами
VBT = ThemeC[,1]/sum(ThemeC[,1])
HugeCl = list(NULL)
Hugeid = c(NULL)
Bborder = 0.25
im=length(VBT[VBT>Bborder])
i=1
j=1
while(i<=im)
{
  jm=length(VBT)
  while(j<=jm)
  {
    if(VBT[j] > Bborder)
    {
      dlist = c(NULL)
      l=1
      lm=length(clasters[,1])
      while(l<=lm)
      {
        if(clasters[l,1] == j)
          dlist[length(dlist)+1] = l
        l=l+1
      }
      HugeCl[[i]] = dlist
      Hugeid[length(Hugeid)+1] = j
      j=j+1
      break
    }
    j=j+1
  }
  i=i+1
}

#собираем все документы хьюджа в датафрейм, кластеризуем все и дополняем кластерс и темеС
HThemeC = c(NULL)
HThemeC[length(HThemeC)+1] = KCounter
Medoids = list(NULL)
Medoids[[1]] = object$medoids
if(length(HugeCl[[1]])>0)
{
  BSublevel = sum(dist.mat)/length(dist.mat) * 1.7
  HPatentlist = data.frame(NULL)
  #KCounter = length(ThemeC[,1])
  i=1
  im=length(HugeCl)
  while(i<=im)
  {
    j=1
    jm=length(HugeCl[[i]])
    while(j<=jm)
    {
      HPatentlist[1,j] = Dpatentlist[Detailing,HugeCl[[i]][j]]
      j=j+1
    }
    pclas <- Corpus(VectorSource(HPatentlist[1,]))
    pclas <- tm_map(pclas, tolower) 
    pclas <- tm_map(pclas, removePunctuation)
    pclas <- tm_map(pclas, removeNumbers)
    pclas <- tm_map(pclas, function(x) removeWords(x, stopwords("english")))
    pclas <- tm_map(pclas, function(x) removeWords(x, AdStops))
    pclas  <- tm_map(pclas, stemDocument, language = "english") 
    #clwords <- TermDocumentMatrix(pclas)
    #clwordsmat = as.matrix(TermDocumentMatrix(pclas))
    Hclwordsmat=unique(as.matrix(TermDocumentMatrix(pclas)))
    #унификация повторений
    ii=1
    iim=length(Hclwordsmat[,1])
    while(ii<=iim)
    {
      j=1
      jm=length(Hclwordsmat[1,])
      while(j<=jm)
      {
        if(Hclwordsmat[ii,j]>1)
          Hclwordsmat[ii,j] = 1
        j=j+1
      }
      ii=ii+1
    }
    #clwordsmat
    HlsaSpace= lsa(Hclwordsmat, dims=dimcalc_ndocs(length(HPatentlist))) #можно и увеличить количество измерений, но надеюсь и так норм
    Hdist.mat = as.matrix(dist(HlsaSpace$d, method = "manhattan"))
    LSAlist[[length(LSAlist)+1]] = HlsaSpace
  
    g=1
    gm=length(Hdist.mat[,1])
    HKCounter = 0
    while(g<=gm)
    {
      if((sum(Hdist.mat[g,])/gm)>=BSublevel)
        HKCounter = HKCounter +1
      g=g+1
    }
    if(HKCounter==0)
      HKCounter=1
    HThemeC[length(HThemeC)+1] = HKCounter
  
    KCounter = KCounter+HKCounter
  
    #кластеризация на основе Clara (лучше по lsaSpace$s)
    object1 = clara(HlsaSpace$d, k=HKCounter, metric = "manhattan", sampsize = length(HlsaSpace$d[,1]), samples = length(HlsaSpace$d[,1])*2) #чем меньше кластеров, тем большая изначально дистанция рассматривается как "эталон", сэмпсайз позволяет изменить это
    Medoids[[i+1]] = object1$medoids
    #Hclasters = as.data.frame(object$clustering)
    
    Hclasters = data.frame(c(HugeCl[[i]]), c(as.data.frame(object1$clustering)[,1]+length(ThemeC[,1])), stringsAsFactors = FALSE)
  
    o=1
    om=length(clasters[,1])
    while(o<=om)
    {
      m=1
      mm=length(Hclasters[,1])
      while(m<=mm)
      {
        if(o == Hclasters[m,1])
        {
          clasters[o,1] = Hclasters[m,2]
        }
        m=m+1
      }
      o=o+1
      setTxtProgressBar(ProgressBar1, o/om*100)
    }
  
    ThemeC=data.frame(NULL)
    tn=1
    while(tn<=KCounter)
    {
      nad=1
      length(clasters)
      tcount=0
      while(nad<=length(clasters[,1]))
      {
        if(clasters[nad,1]==tn)
        {
          tcount=tcount+1
        }
        nad=nad+1
      }
      ThemeC[tn,1] = tcount
      tn=tn+1
      setTxtProgressBar(ProgressBar1, tn/KCounter*100)
    }
    i=i+1
  }
}
HKCounter
KCounter
ThemeC
HugeCl[[1]]

#связи на основе дистанции документов
from = c(NULL)
to = c(NULL)
weight = c(NULL)
Nfrom = c(NULL) #для ни с чем не связанных патентов 
Nto = c(NULL)
Nweight = c(NULL)
i=1
im=length(dist.mat[1,])
while(i<=im)
{
  j=1
  jm=length(dist.mat[1,])
  nmflag = 0
  while(j<=jm)
  {
    if(i<j)
    {
      if(dist.mat[i,j] <= 0.15)
      {
        from[length(from)+1] = i
        to[length(to)+1] = j
        weight[length(weight)+1] = dist.mat[i,j]
        nmflag = 1
      }
    }
    j=j+1
  }
  if(nmflag == 0)
  {
    Nfrom[length(Nfrom)+1] = i
    Nto[length(Nto)+1] = i
    Nweight[length(Nweight)+1] = 0
  }
  i=i+1
  setTxtProgressBar(ProgressBar1, i/im*100)
}

#вставка внешних трендов всех уровней
tr=1
trm=length(trends)
Mto = c(NULL)
Mfrom = c(NULL)
while(tr<=trm)
{
  gw=unique(grep(trends[tr],Dpatentlist[1,], fixed=TRUE))
  if(is.null(length(gw))) #на тот случай, если пустая строка
    length(gw)=0
  if(length(gw)>=1)
  {
    Mto = c(Mto, gw)
    Mfrom = c(Mfrom, rep(Mtrends[tr],length(gw)))
  }
  tr=tr+1
  setTxtProgressBar(ProgressBar1, tr/trm*100)
}

#формирование патентграфа
to=c(to,Nto)
from=c(from,Nfrom)
Weight = c(weight,Nweight)
ConWeight = c(NULL)
patentgraph = data.frame(From = from, To = to, Weigth = Weight)
#temppg=patentgraph
#patentgraph=unique(patentgraph)
i=1
im=length(patentgraph[,1])
while(i<=im)
{
  ConWeight[i]= patentgraph[i,3]#length(temppg[,1][temppg[,2]==patentgraph[i,2]][temppg[,1][temppg[,2]==patentgraph[i,2]]==patentgraph[i,1]])
  i=i+1
  setTxtProgressBar(ProgressBar1, i/im*100)
}
to=c(to,Nto, Mto) #нельзя включать макротренды в патентграф пока, потому что потом на основе него делаем связь тематик
from=c(from,Nfrom,Mfrom)

#подготовка графа связи тематик
themegraph = data.frame(NULL)
Tfrom = c(NULL)
Tto = c(NULL)
TConWeight=c(NULL)
pg = 1
while(pg<=length(patentgraph[,1]))
{
  Tfrom[length(Tfrom)+1] = clasters[as.numeric(patentgraph[pg,1]),1]
  Tto[length(Tto)+1] = clasters[as.numeric(patentgraph[pg,2]),1]
  pgflag = 0
  if(clasters[as.numeric(patentgraph[pg,1]),1] != clasters[as.numeric(patentgraph[pg,2]),1])
  {
    TConWeight[length(TConWeight)+1] = as.numeric(patentgraph[pg,3])
    pgflag = 1
  }
  if(pgflag == 0)
    TConWeight[length(TConWeight)+1] = 0
  pg=pg+1
  setTxtProgressBar(ProgressBar1, pg/length(patentgraph[,1])*100)
}
max(clasters)
themegraph = unique(data.frame(From = Tfrom, To = Tto, stringsAsFactors = FALSE))
temptg=data.frame(From = Tfrom, To = Tto, Weigth = TConWeight, stringsAsFactors = FALSE)
TConWeight = c(NULL)
i=1
im=length(themegraph[,1])
while(i<=im)
{
  S = sum(temptg[,3][temptg[,2]==themegraph[i,2]][temptg[,1][temptg[,2]==themegraph[i,2]]==themegraph[i,1]])
  if(S == 0)
    TConWeight[i] = 0
  if(S > 0)
    TConWeight[i]=S/length(temptg[,1][temptg[,2]==themegraph[i,2]][temptg[,1][temptg[,2]==themegraph[i,2]]==themegraph[i,1]])
  i=i+1
  setTxtProgressBar(ProgressBar1, i/im*100)
}
#TConWeight

#связь тематик и макротрендов
TMfrom = c(NULL)
TMto = c(NULL)
i=1
while(i<=length(Mto))
{
  TMto=c(TMto, clasters[Mto[i],1])
  TMfrom = c(TMfrom,Mfrom[i])
  i=i+1
}
Tfrom = c(Tfrom,TMfrom)
Tto= c(Tto,TMto)
themegraph = data.frame(From = Tfrom, To = Tto, stringsAsFactors = FALSE)
themegraph=unique(themegraph)


#макротренды
#1) определяем веса трендов
trendsfu = data.frame(c(SMtrends), c(trendsSM), stringsAsFactors = FALSE)
trendsfu = unique(trendsfu)
SMtrends = trendsfu[,1]
trendsSM = trendsfu[,2]
NSMtrends = c(NULL)
USMtrends = unique(SMtrends)
q=1
qm=length(USMtrends)
while(q<=qm)
{
  j=1
  jm=length(SMtrends)
  counter = 0
  while(j<=jm)
  {
    if(USMtrends[q] == SMtrends[j])
      counter = counter + 1
    j=j+1
  }
  NSMtrends[q] = counter
  q=q+1
}

#работа с отдельными патентами по отдельным годам
#выделяем патенты по годам с сохранением нумерации для соответствия цветов тематик
AnalyticsFrame=data.frame(NULL)
YearData = as.numeric(substring(Sys.Date(), 0, 4))
#YearData = 2008

if(YearAnalysisFlag == 0)
  i=2
if(YearAnalysisFlag == 1)
  i=1
im = 0
while(i>=im)
{
  Year=YearData-i
  DpatentlistYear = DpatentlistYearSt(Year)
  #Ypatentgraph = YearPatents(DpatentlistYear)
  Ypatentgraph = YearPatents1(DpatentlistYear)
  Ytemppg = Ypatentgraph
  YMacro = YMacros()
  Yto=c(Ypatentgraph[,2], as.character(YMacro[,2]))
  Yfrom=c(Ypatentgraph[,1], as.character(YMacro[,1]))
  Ypatentgraph = data.frame(From = Yfrom, To = Yto, stringsAsFactors = FALSE)
  Ypatentgraph=unique(Ypatentgraph)
  YConWeight = conweightening()
  #creategraph(YMacro[,1], YMacro[,2])
  
  print(paste('Patents', i))
  flush.console()

  #работа с отдельными тематиками по годам
  Ythemegraph = Ypthemegraph()
  Ytemptg=data.frame(From = Ythemegraph[,1], To = Ythemegraph[,2], Weigth = Ythemegraph[,3], stringsAsFactors = FALSE)
  YthemegraphM = YTMacros()
  YTMfrom = c(Ythemegraph[,1],as.character(YthemegraphM[,1]))
  YTMto= c(Ythemegraph[,2],YthemegraphM[,2])
  Ythemegraph = unique(data.frame(From = YTMfrom, To = YTMto, stringsAsFactors = FALSE))
  #Ytemptg=Ythemegraph
  #Ythemegraph=unique(Ythemegraph)
  YTConWeight = tconweightening()
  YThemeC = TWeight() #размеры странные у тематик
  png(paste0('Tgraph',Year), width = 1000, height = 1000, units = 'px')
    createTgraph()
  dev.off()
  Sys.sleep(10)
  
  print(paste('Themes', i))
  flush.console()

  
  trendsW = detectTW()
  trendsCor = detectTC() #здесь могут быть уровни
  
    
  
  trdist.mat = as.matrix(dist(trendsCor, method = "manhattan"))
  trdist.mat[is.na(trdist.mat)] = 0
  #готовим визуализацию и визуализируем
  TrendsGraph = detectTG()
  TrendsCon = TrendsGraph[,3]
  TrendsGraph = data.frame(c(TrendsGraph[,1]), c(TrendsGraph[,2]), stringsAsFactors = FALSE)
 
  
  trendsweight = data.frame(c(USMtrends), c(trendsW), stringsAsFactors = FALSE)
  #визуализация графа трендов
  png(paste0('TRplot',Year), width = 1000, height = 1000, units = 'px')
    plottrends()
  dev.off()
  Sys.sleep(10)
  
  print(paste('Trends', i))
  flush.console()

  if(i == 0)
  {
    #определение количества тем, связанных с макротрендами
    YTCM1 = macroconnect()
    #определение количества связей тем
    YTCt1 = themeconnect()
    #аналитика
    V1 = Analytics(1)
    V2=AnalyticsPro(V1)
    trdist.matL = trdist.mat
    trendsWL = trendsW
  }

  if(i == 1)
  {
    #определение количества тем, связанных с макротрендами
    YTCM2 = macroconnect()
    #определение количества связей тем
    YTCt2 = themeconnect()
    #аналитика
    V3 = Analytics(2) #только после повторения алгоритма выше с предшествующим годом
    V4=AnalyticsPro(V3)
    trdist.matP = trdist.mat
    trendsWP = trendsW
  }

  if(i == 2)
  {
    #определение количества тем, связанных с макротрендами
    YTCM3 = macroconnect()
    #определение количества связей тем
    YTCt3 = themeconnect()
    #аналитика
    V5 = Analytics(3) #если первый был 2018, этот должен быть 2016
    V6=AnalyticsPro(V5)
  }
  i=i-1
}
if(YearAnalysisFlag == 0)
{
  AnalyticsFrame = data.frame(c(t(V1)),c(t(V2)),c(t(V3)),c(t(V4)),c(t(V5)),c(t(V6))) #год, вес тем в году, вес макротрендов (в разбиении) в году
  AnalyticsFrameYTC = data.frame(c(YTCM1),c(YTCM2),c(YTCM3))
  AnalyticsFrameYTCt = list(YTCt1,YTCt2,YTCt3)
}

if(YearAnalysisFlag == 1)
{
  #если только два года в рассмотрении
  AnalyticsFrame = data.frame(c(t(V1)),c(t(V2)),c(t(V3)),c(t(V4))) #год, вес тем в году, вес макротрендов (в разбиении) в году
  AnalyticsFrameYTC = data.frame(c(YTCM1),c(YTCM2))
  AnalyticsFrameYTCt = list(YTCt1,YTCt2)
}

#определяем растущих и сближающихся трендов
trdist.mat = data.frame(trdist.matL-trdist.matP)
trendsnames = USMtrends #names(trdist.mat)
NewMarket = data.frame(NULL)
NMVol = 0
i=1
im=length(trdist.mat[1,])
while(i<=im)
{
  j=i+1
  jm = im
  while(j<=jm)
  {
    if(trdist.mat[i,j]<= -0.3) #границы надо устанваливать 
    {
      if(sum(trdist.matL[i,])>0 && sum(trdist.matP[i,])>0 && sum(trdist.matP[j,])>0 && sum(trdist.matL[j,])>0)
      {
        NMVol = NMVol+1
        NewMarket[NMVol, 1] = trendsnames[i]
        NewMarket[NMVol, 2] = trendsnames[j]
        NewMarket[NMVol, 3] = trdist.mat[i,j]
      }
    }
    j=j+1
  }
  i=i+1
}
trendsWL1 = trendsWL/sum(trendsWL)
trendsWP1 = trendsWP/sum(trendsWP)
trendsW = trendsWL1/trendsWP1
trendsW[is.infinite(trendsW)] = 1.1
trendsW[is.nan(trendsW)] = 0
Gtrends = data.frame(NULL)
GtrVol = 0
i=1
im=length(USMtrends)
while(i<=im)
{
  if(trendsW[i] > 1)
  {
    GtrVol = GtrVol+1
    Gtrends[GtrVol,1] = USMtrends[i]
    Gtrends[GtrVol,2] = trendsW[i]
  }
  i=i+1
}
NewMarket = unique(NewMarket)
Gtrends = unique(Gtrends)

#определение наиболее крупных тем - получаем перечень крупных тем
BigTheme=c(NULL)
startclasters = HThemeC[1] #изначальное количество кластеров
i=2
im=length(HThemeC)
while(i<=im)
{
  j=1
  jm=HThemeC[i]
  AvWeight = length(HugeCl[[i-1]])/jm
  while(j<=jm)
  {
    if(ThemeC[startclasters+j,1] > AvWeight)
      BigTheme[length(BigTheme)+1] = j+startclasters
    j=j+1
  }
  startclasters = startclasters+HThemeC[i]
  i=i+1
}


#определение растущих тем - получаем список растущих тем
grthemes=data.frame(NULL)
growingthemes = AnalyticsFrame[2:(length(AnalyticsFrame[,1])-length(MMtrends)),2]/AnalyticsFrame[2:(length(AnalyticsFrame[,1])-length(MMtrends)),4]
connectedthemes = AnalyticsFrameYTCt[[1]]/AnalyticsFrameYTCt[[2]]
#connectedthemes2 = AnalyticsFrameYTCt[,4]/sum(AnalyticsFrameYTCt[,4])
connectedthemes[is.nan(connectedthemes)]=1 #0
connectedthemes[is.infinite(connectedthemes)]=0.99 #2
connectedthemesA = c(NULL)
i=1
im=length(connectedthemes[,1])
while(i<=im)
{
  SC = sum(connectedthemes[i,][connectedthemes[i,]>0])
  SM = length(connectedthemes[i,][connectedthemes[i,]>0])
  connectedthemesA[i] = SC/SM
  i=i+1
}

growingthemes[is.nan(growingthemes)]=0
growingthemes[is.infinite(growingthemes)]=1.1
i=1
d=0
while(i<=length(growingthemes))
{
  if(growingthemes[i]>1)
  {
    d=d+1
    grthemes[d,1]=i #номер темы
    grthemes[d,2]=growingthemes[i] #темп роста темы
    grthemes[d,3]=connectedthemesA[i]#изменение связей
    if(grthemes[d,3]>1 || grthemes[d,3]==0)
      grthemes[d,4]='dispersing'
    if(grthemes[d,3]==1)
      grthemes[d,4]='stable'#интерпретация изменения связей
    if(grthemes[d,3]<1 && grthemes[d,3]!=0)
      grthemes[d,4]='concentrating'
    grthemes[d,5]=AnalyticsFrameYTC[i,2]#количество макротрендов, которыми поддерживается тема
    grthemes[d,6]=AnalyticsFrameYTC[i,2]/sum(AnalyticsFrameYTC[,2])#AnalyticsFrame[i+1,2] #степень поддержки макротрендами
    grthemes[d,7] = AnalyticsFrame[i+1,2] #относительный размер темы
    grthemes[d,8]='temp'
    grthemes[d,8]=ThemeIdia(i)#содержание темы
  }
  i=i+1
  setTxtProgressBar(ProgressBar1, i/length(growingthemes)*100)
}


#формирование матрицы Крупная тема - статус (рост или нет) - содержание
BigThemeStat=data.frame(NULL)
i=1
while(i<=length(BigTheme))
{
  BigThemeStat[i,1]=BigTheme[i]
  j=1
  BTS='not growing'
  while(j<=length(grthemes[,1]))
  {
    if(BigTheme[i]==grthemes[j,1])
      BTS='growing'
    j=j+1
  }
  BigThemeStat[i,2]=as.character(BTS)
  BigThemeStat[i,3]='temp'
  BigThemeStat[i,3] = ThemeIdia(BigTheme[i])
  i=i+1
}

#группы групп которые растут и растут связи их внутри // растущее количество связей - признак белого пятна // сейчас рассматривается не сближение, а наличие связи в принципе
#если у большой стагнирующей темы начинают расти внешние связи - тренд на преодоление фундаментального барьера
InspectGroups = c(NULL)
i = 1
im = length(grthemes[,1])
while(i<=im) #получаем перечень отдаляющихся групп
{
  if(grthemes[i,4] == 'dispersing')
  {
    InspectGroups[length(InspectGroups)+1] = grthemes[i,1]
  }
  i=i+1
}
BigStag = c(NULL)
if(length(BigTheme)>0)
{
  i = 1
  im = length(BigThemeStat[,1])
  while(i<=im)
  {
  if(BigThemeStat[i,2] == 'not growing')
  {
    BigStag[length(BigStag)+1] = BigThemeStat[i,1]
  }
  i=i+1
}
}
GroupedIns = c(InspectGroups, BigStag)
YearData = as.numeric(substring(Sys.Date(), 0, 4))
#Year=2008
DpatentlistYear = DpatentlistYearSt(Year)
#Ypatentgraph = YearPatents(DpatentlistYear)
Ypatentgraph = YearPatents1(DpatentlistYear)
Ytemppg = Ypatentgraph
YMacro = YMacros()
Yto=c(Ypatentgraph[,2], as.character(YMacro[,2]))
Yfrom=c(Ypatentgraph[,1], as.character(YMacro[,1]))
Ypatentgraph = data.frame(From = Yfrom, To = Yto, stringsAsFactors = FALSE)
#Ytemppg = Ypatentgraph
Ypatentgraph=unique(Ypatentgraph)
YConWeight = conweightening()
Ythemegraph1 = Ypthemegraph()
Connections = data.frame(NULL)
i=1
im=length(GroupedIns)
ConLen=1
while(i<=im)
{
  j=1
  jm=length(Ythemegraph1[,1])
  while(j<=jm)
  {
    if(Ythemegraph1[j,3] > 0)
    {
      lm=length(GroupedIns)
      l=1
      if(GroupedIns[i] == Ythemegraph1[j,1]) 
      {
        while(l<=lm)
        {
          if(GroupedIns[l] == Ythemegraph1[j,2])
          {
            Connections[ConLen,1] = Ythemegraph1[j,1]
            Connections[ConLen,2] = Ythemegraph1[j,2]
            Connections[ConLen,3] = Ythemegraph1[j,3]
            ConLen = ConLen+1
          }
          l=l+1
        }
      }
      if(GroupedIns[i] == Ythemegraph1[j,2])
      {
        while(l<=lm)
        {
          if(GroupedIns[l] == Ythemegraph1[j,1])
          {
            Connections[ConLen,1] = Ythemegraph1[j,1]
            Connections[ConLen,2] = Ythemegraph1[j,2]
            Connections[ConLen,3] = Ythemegraph1[j,3]
            ConLen = ConLen+1
          }
          l=l+1
        }
      }
    }
    j=j+1
    setTxtProgressBar(ProgressBar1, j/jm*100)
  }
  i=i+1
  setTxtProgressBar(ProgressBar1, i/im*100)
}
Ythemegraph = Connections
if(length(Ythemegraph) == 0)
  BRGTflag = 1
if(BRGTflag == 0)
{
  Ytemptg=Ythemegraph
  Ythemegraph=unique(Ythemegraph)
  YthemegraphM = data.frame(NULL)
  YTConWeight = tconweightening()
  RepCon = data.frame(c(Ythemegraph[,1]), c(Ythemegraph[,2]), c(YTConWeight), stringsAsFactors = FALSE)
  RepCon=unique(RepCon)
  RepCon[,3]=RepCon[,3]/2
  i=1
  im=length(RepCon[,1])
  while(i<=im)
  {
    j=1
    jm=length(RepCon[,1])
    while(j<=jm)
    {
      if(RepCon[i,1] == RepCon[j,2] && RepCon[i,2] == RepCon[j,1])
      {
        RepCon[i,3] = RepCon[i,3]+RepCon[j,3]
        RepCon = RepCon[-j,]
        jm=length(RepCon[,1])
        im=length(RepCon[,1])
        if(j>jm)
          break
      }
      else
        j=j+1
    }
    RepCon[i,4] = 'GrowingToNewTheme'
    l=1
    lm=length(BigStag)
    while(l<=lm)
    {
      if(RepCon[i,1] == as.numeric(BigStag[l]) || RepCon[i,2] == as.numeric(BigStag[l]))
        RepCon[i,4] = 'BasicRestriction'
      l=l+1
    }
    i=i+1
  }
  RepCon = data.frame('ThemeID1' = c(RepCon[,1]), 'ThemeID2' = c(RepCon[,2]), 'Inverse intensive of connection' = c(RepCon[,3]), 'Expected shifts' = c(RepCon[,4]), stringsAsFactors = FALSE)
  RepCon=unique(RepCon) #чем ниже интенсивность соединения, тем лучше (меньше дистанция)
  i=1
  im=length(RepCon[,1])
  while(i<=im)
  {
    if(length(BigStag[BigStag == RepCon[i,1]])>0 && length(BigStag[BigStag == RepCon[i,2]])>0)
    {
      RepCon = RepCon[-i,]
      im=length(RepCon[,1])
      if(i>im)
        break
    }
    else
      i=i+1
  }
}
if(BRGTflag == 1)
{
  RepCon = data.frame(NULL)
}

#граф для треугольников и квадратов - растущих тем
ThemeGraphFlag = 1
YConWeight = conweightening()
Ythemegraph = Ypthemegraph()
Ytemptg=Ythemegraph
YthemegraphM = YTMacros()
YTMfrom = c(Ythemegraph[,1],as.character(YthemegraphM[,1]))
YTMto= c(Ythemegraph[,2],YthemegraphM[,2])
Ythemegraph = data.frame(From = YTMfrom, To = YTMto, stringsAsFactors = FALSE)
Ythemegraph=unique(Ythemegraph)
YTConWeight = tconweightening()
YThemeC = TWeight() #размеры странные у тематик
png(paste0('Growing+Conc(st)',Year), width = 5000, height = 5000, units = 'px')
  createTgraph() #для последнего года в анализе
dev.off()
ThemeGraphFlag = 0

#группирование RepCon с отрисовкой
if(BRGTflag == 0)
{
  GTNTgroups = list(NULL)
  GTNTvol = 0
  BRgroups = list(NULL)
  BRvol = 0
  i = 1
  im = length(RepCon[,1])
  while(i<=im)
  {
    if(RepCon[i,4] == "GrowingToNewTheme")
    {
      j=i
      GTNTlist = c(RepCon[i,1], RepCon[i,2])
    
        GTNTvol = GTNTvol+1
        GTNTgroups[[GTNTvol]] = GTNTlist

    }
    if(RepCon[i,4] == "BasicRestriction")
    {
      j=i
      BRlist = c(RepCon[i,1], RepCon[i,2])
    
        BRvol = BRvol+1
        BRgroups[[BRvol]] = BRlist

    }
    i=i+1
    setTxtProgressBar(ProgressBar1, i/im*100)
  }
  ThemeGraphFlag = 2
  png(paste0('WhiteHoles+BR',Year), width = 5000, height = 5000, units = 'px')
    createTgraph() #для последнего года в анализе
  dev.off()
  ThemeGraphFlag = 0
}

#работа с макротрендами по AnalyticsFrame (графики)
TrendsRoleLast1 = sum(AnalyticsFrame[(KCounter+1):length(AnalyticsFrame[,1]),1])/(length(trends)*KCounter) #роль макротрендов в последнем году
TrendsRoleLast2 = sum(AnalyticsFrame[(KCounter+1):length(AnalyticsFrame[,1]),1])/(KCounter)
TrendsRolePrev1 = sum(AnalyticsFrame[(KCounter+1):length(AnalyticsFrame[,1]),3])/(length(trends)*KCounter) #роль макротрендов в предпоследнем году
TrendsRolePrev2 = sum(AnalyticsFrame[(KCounter+1):length(AnalyticsFrame[,1]),3])/(KCounter)
TrendsRoleZero1 = sum(AnalyticsFrame[(KCounter+1):length(AnalyticsFrame[,1]),5])/(length(trends)*KCounter) #роль макротрендов в пред-предпоследнем году
TrendsRoleZero2 = sum(AnalyticsFrame[(KCounter+1):length(AnalyticsFrame[,1]),5])/(KCounter)
TrendsRole = data.frame(c(TrendsRoleLast1, TrendsRoleLast2, TrendsRolePrev1, TrendsRolePrev2,TrendsRoleZero1,TrendsRoleZero2))
Mtrendsstat=data.frame(NULL)
i=1
im=length(SMtrends)
while(i<=im)
{
  Mtrendsstat[i,1] = as.character(MMtrends[i])
  Mtrendsstat[i,2] = as.character(SMtrends[i])
  Mtrendsstat[i,3] = AnalyticsFrame[KCounter+1+i,1]
  Mtrendsstat[i,4] = AnalyticsFrame[KCounter+1+i,3]
  #Mtrendsstat[i,5] = AnalyticsFrame[KCounter+1+i,5]
  i=i+1
}
SupFrame = data.frame(NULL)
SupFV = 0
i=1
im=length(Mtrendsstat[,1])
while(i<=im)
{
  TrS = Mtrendsstat[i,1]
  TrC = 1
  j=i+1
  ii=i
  jm=length(Mtrendsstat[,1])
  while(j<=jm)
  {
    if(Mtrendsstat[j,1] == TrS)
    {
      TrC = TrC+1
      ii=ii+1
    }
    j=j+1
  }
  SupFV = SupFV+1
  SupFrame[SupFV,1] = TrS
  SupFrame[SupFV,2] = TrC
  i=ii+1
}
png(paste0('Macrostat',Year), width = 1000, height = 1000, units = 'px')
  plotMacrostat(3)
dev.off()
png(paste0('Macrostat',Year-1), width = 1000, height = 1000, units = 'px')
  plotMacrostat(4)
dev.off()
#plotMacrostat(5)
Mgr = c(Mtrendsstat[,3]/Mtrendsstat[,4])
Mgr[is.nan(Mgr)] = -1
Mgr[is.infinite(Mgr)] = max(Mgr[Mgr<Inf])
MtrendsstatReport = data.frame(c(Mtrendsstat[,1]), c(Mtrendsstat[,2]), Mgr)
MtrendsstatReport = MtrendsstatReport[MtrendsstatReport[,3]>=0,]
MtrendsstatReport = MtrendsstatReport[order(-MtrendsstatReport[,3]),] #отчет по макротрендам

#граф сближающихся трендов
#1) выделение групп сближающихся трендов листом
NewMarket #тренд 1(имена), тренд 2, изменение дистанции (меньше 0)
if(length(NewMarket)>0)
{
  NewMGroups = list(NULL)
  NMGVol = 0
  i=1
  im=length(NewMarket[,1])
  while(i<=im)
  {
  #j=i
  NMG = c(NewMarket[i,1], NewMarket[i,2])
  
  NMGVol = NMGVol+1
  NewMGroups[[NMGVol]] = NMG
  #}
  i=i+1
  setTxtProgressBar(ProgressBar1, i/im*100)
}
}
#Gtrends #растущие тренды(имена)
#подготовка данных для отрисовки и отрисовка
trendsW = detectTW()
trendsCor = detectTC() #здесь могут быть уровни
trdist.mat = as.matrix(dist(trendsCor, method = "manhattan"))
trdist.mat[is.na(trdist.mat)] = 0
#готовим визуализацию и визуализируем
TrendsGraph = detectTG()
TrendsCon = TrendsGraph[,3]
TrendsGraph = data.frame(c(TrendsGraph[,1]), c(TrendsGraph[,2]), stringsAsFactors = FALSE)
trendsweight = data.frame(c(USMtrends), c(trendsW), stringsAsFactors = FALSE)
#визуализация графа трендов
trendsgraphflag = 1
png(paste0('TRplotA',Year), width = 1000, height = 1000, units = 'px')
  plottrends()
dev.off()
trendsgraphflag = 0

# #Интегральный индекс и лист технологических трендов

#отчет 1 - про технологические тренды
# индекс считается как темп роста темы (растущей темы или среднее из растущих) умноженный на критерий статуса (1 - белые пятна, 1,5 - концентрирующиеся, 1,7 - фундаментальные барьеры)
# формат отчета - ТОП-15 (темы, статус, значение индекса, содержание)
KG = 1
KGs = 0.8
KW = 1.5
KBR = 1.7
PreIndex = data.frame(NULL)
PreIndexVol = 0
j=1
jm=4
while(j<=jm)
{
  i=1
  if(j <= 2)
    im = length(grthemes[,1])
  if(j > 2)
    im = length(RepCon[,1])
  while(i<=im)
  {
    if(j == 1)
    {
      if(grthemes[i,4] == 'concentrating')
      {
        PreIndexVol = PreIndexVol + 1
        PreIndex[PreIndexVol,1] = grthemes[i,1]
        PreIndex[PreIndexVol,2] = 'growing'
        PreIndex[PreIndexVol,3] = 'concentrating'
        PreIndex[PreIndexVol,4] = grthemes[i,2] * KG
      }
    }
    if(j == 2)
    {
      if(grthemes[i,4] == 'stable')
      {
        PreIndexVol = PreIndexVol + 1
        PreIndex[PreIndexVol,1] = grthemes[i,1]
        PreIndex[PreIndexVol,2] = 'growing'
        PreIndex[PreIndexVol,3] = 'stable'
        PreIndex[PreIndexVol,4] = grthemes[i,2] * KGs
      }
    }
    if(j == 3)
    {
      if(BRGTflag == 0)
      {
        if(RepCon[i,4] == 'GrowingToNewTheme')
        {
          PreIndexVol = PreIndexVol + 1
          PreIndex[PreIndexVol,1] = as.numeric(RepCon[i,1])
          PreIndex[PreIndexVol,2] = as.numeric(RepCon[i,2])
          PreIndex[PreIndexVol,3] = 'GrowingToNewTheme'
          PreIndex[PreIndexVol,4] = (grthemes[grthemes[,1] == RepCon[i,1],2] + grthemes[grthemes[,1] == RepCon[i,2],2]) * 0.5 * KW
        }
      }
    }
    if(j == 4)
    {
      if(BRGTflag == 0)
      {
        if(RepCon[i,4] == 'BasicRestriction')
        {
          PreIndexVol = PreIndexVol + 1
          PreIndex[PreIndexVol,1] = as.numeric(RepCon[i,1])
          PreIndex[PreIndexVol,2] = as.numeric(RepCon[i,2])
          PreIndex[PreIndexVol,3] = 'BasicRestriction'
          if(length(grthemes[grthemes[,1] == RepCon[i,1],2])==0)
            BRgw = grthemes[grthemes[,1] == RepCon[i,2],2]
          if(length(grthemes[grthemes[,1] == RepCon[i,1],2])>0)
            BRgw = grthemes[grthemes[,1] == RepCon[i,1],2]
          PreIndex[PreIndexVol,4] = BRgw * KBR
        }
      }
    }
    i=i+1
  }
  j=j+1
}
PreIndex = PreIndex[order(-PreIndex[,4]),]

TIndex = data.frame(NULL)
TIVol = 0
i=1
im=15
if(length(PreIndex[,1])<15)
  im = length(PreIndex[,1])
while(i<=im)
{
  TIVol = TIVol+1
  TIndex[TIVol,1] = PreIndex[i,1]
  TIndex[TIVol,2] = '-'
  if(is.na(as.numeric(PreIndex[i,2]))==FALSE)
    TIndex[TIVol,2] = PreIndex[i,2]
  TIndex[TIVol,3] = PreIndex[i,4]
  TIndex[TIVol,4] = 'temp'
  TIndex[TIVol,4] = ThemeIdia(PreIndex[i,1])
  TIndex[TIVol,5] = '-'
  if(is.na(as.numeric(PreIndex[i,2]))==FALSE)
    TIndex[TIVol,5] = ThemeIdia(as.numeric(PreIndex[i,2]))
  TIndex[TIVol,6] = PreIndex[i,3]
  i=i+1
}

#отчет 2 - про рыночные изменения
#информация о макротрендах: свод (граф) и визуализация - количественная оценка значимости трендов
NM=0.5
GM=1
MarketRep = data.frame(NULL)
MarketRepVol=0
trendsW = trendsWL1/trendsWP1
trendsW[is.infinite(trendsW)] = 1.1
trendsW[is.nan(trendsW)] = 0
USMtrends
j=1
jm=2
while(j<=jm)
{
  i=1
  if(j==1)
    im=length(Gtrends[,1])
  if(j==2)
    im=length(NewMarket[,1])
  while(i<=im)
  {
    if(j==1)
    {
      MarketRepVol = MarketRepVol + 1
      MarketRep[MarketRepVol,1] = Gtrends[i,1]
      MarketRep[MarketRepVol,2] = 'Growing'
      MarketRep[MarketRepVol,3] = '-'
      MarketRep[MarketRepVol,4] = Gtrends[i,2]*GM
    }
    if(j==2)
    {
      MarketRepVol = MarketRepVol + 1
      MarketRep[MarketRepVol,1] = NewMarket[i,1]
      MarketRep[MarketRepVol,2] = NewMarket[i,2]
      MarketRep[MarketRepVol,3] = 'NewMarket'
      MarketRep[MarketRepVol,4] = (trendsW[USMtrends==NewMarket[i,1]]+trendsW[USMtrends==NewMarket[i,2]])*0.5*(0-NewMarket[i,3])*NM
    }
    i=i+1
  }
  j=j+1
}
MarketRep = unique(MarketRep)
MarketRep = MarketRep[order(-MarketRep[,4]),]

MIndex = data.frame(NULL)
MIVol = 0
i=1
im=15
if(length(MarketRep[,1])<15)
  im = length(MarketRep[,1])
while(i<=im)
{
  MIVol = MIVol+1
  MIndex[MIVol,1] = MarketRep[i,1]
  if(MarketRep[i,3]=='-')
    MIndex[MIVol,2] = MarketRep[i,3]
  else
    MIndex[MIVol,2] = MarketRep[i,2]
  MIndex[MIVol,3] = MarketRep[i,4]
  i=i+1
}

#отчет 3 - сводный отчет
FinalList = data.frame(NULL)
FinalListVol = 0
i=1
im=length(PreIndex[,1])
while(i<=im)
{
  if(PreIndex[i,2] == 'growing')
  {
    TRW = 1
    FinalListVol = FinalListVol+1
    SupByTrends = SBT(PreIndex[i,1])
    #png(paste0('TrendsDependence',PreIndex[i,1]), width = 1000, height = 1000, units = 'px')
    #  plotGroupDependence()
    #dev.off()
    SBTframe = data.frame(c(USMtrends), c (SupByTrends), stringsAsFactors = FALSE)
    SBTframe = SBTframe[SBTframe[,2]>0,]
    FinalList[FinalListVol,1] = PreIndex[i,1]
    FinalList[FinalListVol,2] = PreIndex[i,2]
    FinalList[FinalListVol,3] = PreIndex[i,3]
    j=1
    jm=length(SBTframe[,1])
    while(j<=jm)
    {
      l=1
      lm=length(MarketRep[,1])
      while(l<=lm)
      {
        if(MarketRep[l,3] == "NewMarket")
        {
          if(length(SBTframe[SBTframe[,1] == MarketRep[l,1],1]) > 0 && length(SBTframe[SBTframe[,1] == MarketRep[l,2],1]) > 0)
            TRW = TRW + MarketRep[l,4]
        }
        else
        {
          if(MarketRep[l,1] == SBTframe[j,1])
            TRW = TRW + MarketRep[l,4]
        }
        l=l+1
      }
      j=j+1
    }
    FinalList[FinalListVol,4] = TRW*PreIndex[i,4]
  }
  else
  {
    TRW1 = 1
    TRW2 = 1
    FinalListVol = FinalListVol+1
    SupByTrends = SBT(PreIndex[i,1])
    #png(paste0('TrendsDependence',PreIndex[i,1]), width = 1000, height = 1000, units = 'px')
    #  plotGroupDependence()
    #dev.off()
    SupByTrends1 = SupByTrends
    SBTframe1 = data.frame(c(USMtrends), c (SupByTrends1), stringsAsFactors = FALSE)
    SBTframe1 = SBTframe1[SBTframe1[,2]>0,]
    SupByTrends = SBT(PreIndex[i,2])
    #png(paste0('TrendsDependence',PreIndex[i,2]), width = 1000, height = 1000, units = 'px')
    #  plotGroupDependence()
    #dev.off()
    SupByTrends2 = SupByTrends
    SBTframe2 = data.frame(c(USMtrends), c (SupByTrends2), stringsAsFactors = FALSE)
    SBTframe2 = SBTframe2[SBTframe2[,2]>0,]
    FinalList[FinalListVol,1] = PreIndex[i,1]
    FinalList[FinalListVol,2] = PreIndex[i,2]
    FinalList[FinalListVol,3] = PreIndex[i,3]
    j=1
    jm=length(SBTframe1[,1])
    while(j<=jm)
    {
      l=1
      lm=length(MarketRep[,1])
      while(l<=lm)
      {
        if(MarketRep[l,3] == "NewMarket")
        {
          if(length(SBTframe1[SBTframe1[,1] == MarketRep[l,1],1]) > 0 && length(SBTframe1[SBTframe1[,1] == MarketRep[l,2],1]) > 0)
            TRW1 = TRW1 + MarketRep[l,4]
        }
        else
        {
          if(MarketRep[l,1] == SBTframe1[j,1])
            TRW1 = TRW1 + MarketRep[l,4]
        }
        l=l+1
      }
      j=j+1
    }
    j=1
    jm=length(SBTframe2[,1])
    while(j<=jm)
    {
      l=1
      lm=length(MarketRep[,1])
      while(l<=lm)
      {
        if(MarketRep[l,3] == "NewMarket")
        {
          if(length(SBTframe2[SBTframe2[,1] == MarketRep[l,1],1]) > 0 && length(SBTframe2[SBTframe2[,1] == MarketRep[l,2],1]) > 0)
            TRW2 = TRW2 + MarketRep[l,4]
        }
        else
        {
          if(MarketRep[l,1] == SBTframe2[j,1])
            TRW2 = TRW2 + MarketRep[l,4]
        }
        l=l+1
      }
      j=j+1
    }
    TRW = (TRW1+TRW2)*0.5
    FinalList[FinalListVol,4] = TRW*PreIndex[i,4]
  }
  i=i+1
  setTxtProgressBar(ProgressBar1, i/im*100)
}
FinalList = FinalList[order(-FinalList[,4]),]

i=1
im=length(PreIndex[,1])
while(i<=im*2)
{
  if(i<=im)
    i1 = 1
  else
    i1 = 2
  if(is.numeric(PreIndex[i,i1]))
  if(is.na(is.numeric(PreIndex[i,i1])) == FALSE)
  {
    SupByTrends = SBT(PreIndex[i,i1])
    #png(paste0('TrendsDependence',PreIndex[i,i1]), width = 1000, height = 1000, units = 'px')
      plotGroupDependence()
    #dev.off()
    #Sys.sleep(60)
      ggsave(paste0('TrendsDependence',PreIndex[i,i1]), plot = plotGroupDependence(), device = "png", path = NULL,
             scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
             dpi = 300, limitsize = TRUE)
    print(paste('Draw', PreIndex[i,i1]))
    flush.console()
  }
  i=i+1
}


FIndex = data.frame(NULL)
FIVol = 0
i=1
im=15
if(length(FinalList[,1])<15)
  im = length(FinalList[,1])
while(i<=im)
{
  FIVol = FIVol+1
  FIndex[FIVol,1] = FinalList[i,1]
  if(is.na(as.numeric(FinalList[i,2])) == FALSE)
    FIndex[FIVol,2] = FinalList[i,2]
  if(is.na(as.numeric(FinalList[i,2])))
    FIndex[FIVol,2] = '-'
  FIndex[FIVol,3] = FinalList[i,4]
  FIndex[FIVol,4] = 'temp'
  FIndex[FIVol,4] = ThemeIdia(FinalList[i,1])
  FIndex[FIVol,5] = '-'
  if(is.na(as.numeric(FinalList[i,2])) == FALSE)
    FIndex[FIVol,5] = ThemeIdia(FinalList[i,2])
  FIndex[FIVol,6] = FinalList[i,3]
  i=i+1
}
#подгружаем в документ сохраненные картинки и таблицы
#графика отчета
getwd()

write.csv(MIndex, file = "/Users/romansmirnov/Downloads/MIndex.csv", row.names = FALSE)
write.csv(TIndex, file = "/Users/romansmirnov/Downloads/TIndex.csv", row.names = FALSE)
write.csv(FIndex, file = "/Users/romansmirnov/Downloads/FIndex.csv", row.names = FALSE)
write.csv(clasters, file = "/Users/romansmirnov/Downloads/clasters.csv", row.names = FALSE)
write.csv(DpatentlistYear, file = "/Users/romansmirnov/Downloads/DpatentlistYear.csv", row.names = FALSE)
write.csv(trendsfu, file = "/Users/romansmirnov/Downloads/trendsfu.csv", row.names = FALSE)
write.csv(USMtrends, file = "/Users/romansmirnov/Downloads/USMtrends.csv", row.names = FALSE)
write.csv(share, file = "/Users/romansmirnov/Downloads/share.csv", row.names = FALSE)
write.csv(TrendsRole, file = "/Users/romansmirnov/Downloads/TrendsRole.csv", row.names = FALSE)
write.csv(SRequest, file = "/Users/romansmirnov/Downloads/SRequest.csv", row.names = FALSE)

