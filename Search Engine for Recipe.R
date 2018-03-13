rm(list = ls())
library(tm)
library(SnowballC)
library(tidytext)
library(dplyr)
library(reshape2)
library(tidyverse)
library(lsa)
require(lubridate)
require(plotly)
require(shinydashboard)
require(shiny)
require(ggplot2)
require(scales)
require(lubridate)
require(qpcR)
require(shinyBS)
require(DT)
require(rvest)
require(dplyr)
require(stringr)
library(tidyr)
require("coreNLP")
require("pander")
require("syuzhet")
library(qdapRegex)
# 
# #########################################
#  url <- "http://thewoksoflife.com/recipe-list/"
#  html <- url%>% read_html()%>%html_nodes(".entry-content li a")%>%html_attr("href")
# 
#  df <- data.frame(matrix(NA,nrow = 928,ncol = 2))
#  colnames(df) <- c("Name","Ingredients")
#  total.time <- c()
#  for(i in 1:928){
#    ## recipe name
#    a <- ex_between(html[i], "/","/")[[1]][5]
#    df[i,1] <- gsub("-"," ",a)
#    ## ingredients
#    ingredient <- html[i]%>% read_html()%>%html_nodes(".ingredient")%>%html_text()
#    df[i,2] <- paste(ingredient,sep = "" ,collapse = "")
#    ## prepare time
#    b <- html[i]%>% read_html()%>%html_nodes(".ERSTimeRight+ .ERSTimeRight time")%>%html_text()
#    # grep("+hour", b,perl=TRUE, value=TRUE)
#    time <- b %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
#    if (length(time)==0){total.time[i] <- NA}else{
#      if (grepl("hour",b)==T&grepl("mins",b)==T){
#        total.time[i] <- time[1]+time[2]/60
#      }else if (grepl("hour",b)==T&grepl("mins",b)==F){
#        total.time[i] <- time[1]
#      }else if (grepl("hour",b)==F&grepl("mins",b)==T){
#        total.time[i] <- time[1]/60
#      }
# 
#    }
#    print(i)
#  }
#  df <- cbind(html[1:928],df,total.time)
#  colnames(df)[1] <- "HTML"
#  df <- df[!duplicated(df),]
#  s <- list(length=648)
#  df$HTML <- as.character(df$HTML)
#  for(i in 1:648){
#    # grab the reviews for each recipe
#    s[[i]] <- df$HTML[i]%>% read_html()%>%html_nodes(".depth-1 > article .comment-content p")%>%html_text()
#  }
# 
#  setwd("C:/Text mining/Final Project")
#  hu.liu.pos <- scan('positive-words.txt', what='character', comment.char=';')
#  hu.liu.neg <- scan('negative-words.txt', what='character', comment.char=';')
# 
#  ## check whether some important in liu's dictionary
#  # which(hu.liu.pos%in%"delicious")
#  # which(hu.liu.neg%in%"long")
#  # Add a few twitter and industry favorites
#  pos.words <- c(hu.liu.pos, "can't wait","can not wait ")
#  neg.words <- c(hu.liu.neg, "long")
# 
#  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
#  {
#    require(plyr)
#    require(stringr)
# 
#    scores = laply(sentences, function(sentence, pos.words, neg.words) {
# 
#      # clean up sentences with R's regex-driven global substitute, gsub():
#      ## remove non
#      sentence <- stringr::str_replace_all(sentence,"[^a-zA-Z\\s]", " ")
#      sentence = gsub('[[:punct:]]', '', sentence)
#      sentence = gsub('[[:cntrl:]]', '', sentence)
#      sentence = gsub('\\d+', '', sentence)
#      # and convert to lower case:
#      sentence = tolower(sentence)
# 
#      # split into words. str_split is in the stringr package
#      word.list = str_split(sentence, '\\s+')
#      # sometimes a list() is one level of hierarchy too much
#      words = unlist(word.list)
# 
#      # compare our words to the dictionaries of positive & negative terms
#      pos.matches = match(words, pos.words)
#      neg.matches = match(words, neg.words)
# 
#      # match() returns the position of the matched term or NA
#      # we just want a TRUE/FALSE:
#      pos.matches = !is.na(pos.matches)
#      neg.matches = !is.na(neg.matches)
# 
#      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
#      score = sum(pos.matches) - sum(neg.matches)
# 
#      return(score)
#    }, pos.words, neg.words, .progress=.progress )
# 
#    scores.df = data.frame(score=scores, text=sentences)
#    return(scores.df)
#  }
# 
#  # Score the sentiments for each review of each recipe
#  for(i in 1:648){
#    df$score.sentiment[i] <-score.sentiment(s[[i]],  pos.words, neg.words, .progress='text')$score%>%mean()%>%round(.,1)
#    print(i)
#  }
# 
#  ## major ingredients: chicken, beef, turkey, egg, fish, seafood, shrimp,pork,tofu
#  for(i in 1:648){
#    if(grepl("nut",df$Name[i])==T){
#      df$category[i] <- "nuts"
#    }else if(grepl("noodles",df$Name[i])==T|grepl("dumpling",df$Name[i])==T){
#      df$category[i] <- "noodle/dumpling"
#    }else if (grepl("pancake",df$Name[i])==T|grepl("cake",df$Name[i])==T|grepl("dumpling",df$Name[i])==T){
#      df$category[i] <- "dessert/cake"
#    }else if (grepl("lamb",df$Name[i])==T){
#      df$category[i] <- "lamb"
#    }else if (grepl("beef",df$Name[i])==T|grepl("steak",df$Name[i])==T){
#      df$category[i] <- "beef"
#    }else if(grepl("turkey",df$Name[i])==T){
#      df$category[i] <- "turkey"
#    }else if(grepl("shrimp",df$Name[i])==T|grepl("fish",df$Name[i])==T|grepl("seafood",df$Name[i])==T){
#      df$category[i] <- "seafood"
#    }else if(grepl("pork",df$Name[i])==T|grepl("rib",df$Ingredients[i])==T){
#      df$category[i] <- "pork"
#    }else if(grepl("tofu",df$Name[i])==T){
#      df$category[i] <- "tofu"
#    }else if(grepl("chicken",df$Name[i])==T){
#      df$category[i] <- "chicken"
#    }else if(grepl("eggs",df$Name[i])==T|grepl("tea eggs",df$Name[i])==T){
#      df$category[i] <- "egg"
#    }else if(grepl("cabbage",df$Name[i])==T|grepl("eggplant",df$Name[i])==T|grepl("broccoli",df$Name[i])==T|
#             grepl("tea",df$Name[i])==T){
#      df$category[i] <- "vegetable/drinks"
#    }else if (grepl("beef",df$Ingredients[i])==T|grepl("steak",df$Ingredients[i])==T){
#      df$category[i] <- "beef"
#    }else if(grepl("turkey",df$Ingredients[i])==T){
#      df$category[i] <- "turkey"
#    }else if(grepl("fish",df$Ingredients[i])==T|grepl("seafood",df$Ingredients[i])==T|grepl("shrimp",df$Ingredients[i])==T){
#      df$category[i] <- "seafood"
#    }else if(grepl("pork",df$Ingredients[i])==T|grepl("rib",df$Ingredients[i])==T){
#      df$category[i] <- "pork"
#    }else if(grepl("tofu",df$Ingredients[i])==T){
#      df$category[i] <- "tofu"
#    }else if(grepl("chicken",df$Ingredients[i])==T){
#      df$category[i] <- "chicken"
#    }else if(grepl("pancake",df$Ingredients[i])==T|grepl("flour",df$Ingredients[i])==T){
#      df$category[i] <- "dessert/cake"
#  }else if(grepl("tea eggs",df$Ingredients[i])==T|grepl("eggs",df$Ingredients[i])==T){
#      df$category[i] <- "egg"
#    }else{
#      df$category[i] <- "vegetable/drinks"
#    }
#    print(i)
#  }
#  ## summary the number of reviews
#  for(i in 1:648){
#    df$number_reviews[i] <- length(s[[i]])
#  }
# ## make category as a factor variable
#  df$category <- as.factor(df$category)
#  df1 <- aggregate(df$number_reviews,by=list(df$category),mean)
#  colnames(df1) <- c("category","mean.number_reviews")
#  df2 <- df[!is.na(df$score.sentiment),]
#  df3 <- aggregate(df2$score.sentiment,by=list(df2$category),mean)
#  colnames(df3) <- c("category","score.sentiment")
#  df.category <- merge(df1,df3,by="category",all = T)
#  output <- list(df,df.category)
#  saveRDS(output, file = "finalproject.rds")

setwd("C:/Text mining/Final Project")
output <- readRDS(file = "finalproject.rds")
df <- output[[1]]
df.category <- output[[2]]
#############################################
ui <- dashboardPage(
  ## create Header
  dashboardHeader(
    title="Search Engine for Chinese Food Recipes",
    titleWidth = 600),
  dashboardSidebar(
  textInput("include","Ingredient Search","chicken"),
  textInput("exclude","Exclude Ingredient","ginger"),
  textInput("time","Maximum Hour","1"),
  submitButton("searchbutton","Search")
  ),
  dashboardBody(tabsetPanel(
    navbarMenu(
      title = "Selected Recipes",
      tabPanel("Selected Recipes",
               fluidRow(
                 column(width = 12,
                        dataTableOutput("table1")
                 )
                 )
               )
      ),
    navbarMenu(
      title = "Ingredient Analysis",
      tabPanel("Ingredient Analysis",
               fluidRow(
                 column(width = 12,
                        plotOutput("plot1")
                 ),
                 column(width = 12,
                        plotOutput("plot2")
                        
                 )
               )
      )
    ))))

server <- function(input,output){
  run.model <- reactive({
    t1 <- Sys.time()
    ## filter recipes by excluded ingredient
    df.length <- nrow(df)
    for(i in 1:df.length){
      if(grepl(input$exclude,df$Ingredients[i],perl = T)==T){
        df <- df[-i,]
      }
    }
    print("exclude")
    time <- as.numeric(input$time)
    print("time")
    ## filter recipes by time
    if(length(time)>0){
      df <- df[!is.na(df$total.time),]
      df <- df[as.numeric(df$total.time)<=time,]
    }
    print(df$total.time[length(df$total.time)])
    row.names(df) <- 1:nrow(df)
    ## the index of the include document 
    include.index <- nrow(df)+1
    ## remove all numbers
    df3 <- stringr::str_replace_all(df$Ingredients,"[^a-zA-Z\\s]", " ")
    my.docs <- VectorSource(c(df3,input$include))
    
    text <- Corpus(my.docs)
    text2 <- tm_map(text,removePunctuation)
    text3 <- tm_map(text2, removeNumbers)
    
    dtm <- DocumentTermMatrix(text3,
                              control = list(
                                weighting = function(x) weightTfIdf(x, normalize = FALSE)
                              ))
    
    ## select documents have the query terms, extract document ID which has sum.tfidf >0,
    ## use unlist to separate query to individual character
    sum.tfidf <- apply(dtm[,colnames(dtm)%in%unlist(strsplit(input$include," "))],1,sum)
    # print(unlist(strsplit(input$include," ")))
    doc <- which(sum.tfidf>0)
    doc <- unique(doc)
    l <- length(which(sum.tfidf>0))
    ## exclude the last document, which is the input 
    l.length <- l-1
    ### cosine similiarity
    print("cosine similiarity")
    dff <- data.frame(document=character(),cosine_similarity=numeric(),stringsAsFactors = F)
    
    ## exclude the last document, because the last document is query.
    for (j in 1:l.length){
      ## column 1 for document ID
      dff[j,1] <- doc[j]
      ## convert each column of tdm to vector
      dff[j,2] <- cosine(as.vector(dtm[include.index,]),as.vector(dtm[doc[j],]))
      print("here 3")
      print(j)
    }
    ###### ranking cosine similarity, select documnet names of top 20
    top10_doc <- dff[order(dff[,2],decreasing = T),][1:20,]
    top10.records <- df[as.numeric(top10_doc$document),]
    top10.records$total.time <- round(top10.records$total.time,1)
    top10.records <- top10.records[,1:5]
    rownames(top10.records) <- NULL
    print("top10")
    df.category <- df.category
    outdata=list(data1=top10.records,
                 data2=df.category)

    print(Sys.time()-t1)
    return(outdata)
  })
  output$table1 <- renderDataTable(data.frame(run.model()$data1))
  output$plot1 = renderPlot({
    
    data.plot = run.model()$data2
    ggplot(data = data.plot,
           aes(x = category,
               y = mean.number_reviews)) +
      geom_point(size = 5)+labs(x="Recipe Category",y="Mean of the number of reviews")+
      theme(axis.text.y=element_text(size=18), 
            axis.text.x=element_text(size=18), 
            axis.title.x = element_text(size=18, face="bold"),
            axis.title.y = element_text(size=18, face="bold"),
            legend.position="bottom", legend.title=element_blank(),
            legend.text=element_text(size=14))
  })
  output$plot2 = renderPlot({
    
    data.plot = run.model()$data2
    ggplot(data = data.plot,
           aes(x = category,
               y = score.sentiment)) +
      geom_point(size = 5)+labs(x="Recipe Category",y="Mean of sentiment scores")+
      theme(axis.text.y=element_text(size=18), 
            axis.text.x=element_text(size=18), 
            axis.title.x = element_text(size=18, face="bold"),
            axis.title.y = element_text(size=18, face="bold"),
            legend.position="bottom", legend.title=element_blank(),
            legend.text=element_text(size=14))
  })
  
}

runApp(list(ui=ui,server=server))














