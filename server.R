library(shiny)
library(pdftools)
library(tm)
library(udpipe)
library(tidytext)
library(dplyr)
library(wordcloud)
library(reshape2) #melt
library(shinyFiles)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(data.table)
library(stringr)
library(rowr) #for cbind.fill
library(reshape2) #melt

ud_model <- udpipe_load_model('G:/TEMP/Kenneth/Data Science Applications/PDF Extraction App')

server <- shinyServer(function(input, output) {
  
  # dir
  shinyDirChoose(input, 'dir', roots = getVolumes())
  
  dir <- reactive({
    return(print(parseDirPath(getVolumes(), input$dir)))
  })
  
  mypdf1_list<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      
      withProgress({
        setProgress(message = "Extracting Text...")
        
        lst=list()
        for(i in 1:length(inFile[,1])){
          lst[[i]] <- pdf_text(inFile[[i, 'datapath']])
        }
        
        lst
      })
    }
  }) 
  
  mypdf2_list<-reactive({
    
    inFile <- input$file2
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      
      withProgress({
        setProgress(message = "Extracting Text...")
        
        lst=list()
        for(i in 1:length(inFile[,1])){
          lst[[i]] <- pdf_text(inFile[[i, 'datapath']])
        }
        
        lst
      })
    }
  }) 
  
  corp <- reactive({
    x=c(unlist(mypdf1_list()))
    
    corp <- Corpus(VectorSource(x))
    corp <- tm_map(corp, content_transformer(tolower))

    return(corp)   
    
  })
  corp2 <- reactive({
    
    x=c(unlist(mypdf2_list()))
    
    corp <- Corpus(VectorSource(x))
    corp <- tm_map(corp, content_transformer(tolower))
    
    return(corp)   
    
  })
  
  
  x <- reactive({ 
    
  x <- corp()$content
  
  x <- udpipe_annotate(ud_model, x = x)
  x <- as.data.frame(x)
  
  return(x)
  })
  
  gr <- reactive({
    
    gr <- lapply(mypdf1_list(), pdf_text)
    
    gr <- do.call(rbind.data.frame, gr)
    gr <- as.data.frame(do.call(paste, as.data.frame(gr, stringsAsFactors=FALSE)))
    gr$doc <- mypdf2_list()
    gr$doc <- trimws(gsub("\\s+", " ", str_trim(gr$doc)))
    gr$text <- tolower(gr$text)
  })
  
  gr2 <- reactive({
    
    gr <- lapply(mypdf2_list(), pdf_text)
    
    gr <- do.call(rbind.data.frame, gr)
    gr <- as.data.frame(do.call(paste, as.data.frame(gr, stringsAsFactors=FALSE)))
    gr$doc <- mypdf2_list()
    gr$doc <- trimws(gsub("\\s+", " ", str_trim(gr$doc)))
    gr$text <- tolower(gr$text)
  })
  
  output$RAKE <- DT::renderDataTable(server = FALSE,{
    
    x <- x()
    
    stats <- keywords_rake(x = x, 
                           term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                           relevant = x$upos %in% c("NOUN", "ADJ"),
                           ngram_max = 4)
    
    DT::datatable(stats,
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  output$phrases <- DT::renderDataTable(server = FALSE,{
    
    x <- x()
    
    x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
    
    stats5 <- keywords_phrases(x = x$phrase_tag, term = x$token, 
                               pattern = "(A|N)+N(P+D*(A|N)*N)*", 
                               is_regex = TRUE, ngram_max = 4, detailed = FALSE)

    DT::datatable(stats5,
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  output$depndprsing <- DT::renderDataTable(server = FALSE,{
    
    x <- x()
    
    stats6 <- merge(x, x, 
                    by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                    by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                    all.x = TRUE, all.y = FALSE, 
                    suffixes = c("", "_parent"), sort = FALSE)
    stats6 <- subset(stats6, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
    stats6$term <- paste(stats6$lemma_parent, stats6$lemma, sep = " ")
    stats6 <- txt_freq(stats6$term)
    
    DT::datatable(stats6,
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    
  })
  
  keywords <- reactive({
    
    if (input$submit > 0) {
      
      df=isolate(unlist(strsplit(input$words,",")))
      
      #df <- c(isolate(input$words))

      return(df)
    }
  })
  
  kw <- reactive({
    
    kw <- read.csv("./www/keywords.csv", header=FALSE, stringsAsFactors=FALSE)
    kw <- data.frame(lapply(kw, as.character), stringsAsFactors=FALSE)
    names(kw) <- lapply(kw[1, ], as.character)
    kw <- kw[-1,]
    kw[kw==""] <- NA 
    
    return(kw)

  })
  
  output$list <- DT::renderDataTable({
    kw <- data.frame(kw())
    
    DT::datatable(kw,
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  output$kwtable <- DT::renderDataTable({
    
    keywords <- keywords()
    
    OldPhrases <- data.frame()
    
    y <- corp()$content
    
    for (i in unique(keywords)){
      
      isolate({
      
      pattern <- paste0("([^\\s]+\\s){0,5}",i,"(\\s[^\\s]+){0,5}")
      
      NewPhrases <- data.frame(stringr::str_extract(y, pattern))
      
      Phrases <- rbind(setDT(OldPhrases), setDT(NewPhrases), fill=T)
      
      colnames(Phrases) <- 'Picked Phrases'

      })
      
    }
    
    
    Phrases <- na.omit(Phrases)
    
    DT::datatable(Phrases,
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  output$kwclass <- DT::renderDataTable({
    
    gr <- corp()$content
    
    kw <- kw()
    
    kws <- c("Slab","footwall","gravel", "plantfeed","oversize", "thickness", "texture", colnames(kw))
    kws <- tolower(kws)

    Phrases <- data.frame()
    
    #pick Phrases with keywords

    for (x in unique(kws)){
      
      pattern <- paste0("[^.]*",x,"[^.]*\\.")
      NewPhrases <- data.frame(stringr::str_extract(gr, pattern))
      names(NewPhrases) <- "x"
      Phrases <- rbind(setDT(Phrases), setDT(NewPhrases), fill = T)
      Phrases <- na.omit(Phrases)
    }
    
    Phrases <- data.frame(lapply(Phrases, as.character), stringsAsFactors=FALSE)
    
    colnames(Phrases) <- 'Picked Phrases'
    
    NewPhrases <- data.frame("x"=character(0),"stat"=character(0))
    n <- matrix(1:ncol(kw))
    nphrases <- matrix(1:nrow(Phrases))
    
    for (i in unique(n)){
      
      k <- c(as.character(colnames(kw)[i]),unlist(strsplit((colnames(kw)[i]), " "))[1], as.character(kw[,i]))

      for (j in unique(k)){

        pattern <- paste0("([^\\s]+\\s){0,0}",j,"(\\s[^\\s]+){0,0}")
        
        for (l in unique(nphrases)){
          
          if (grepl(k[1], Phrases[l,1]) | grepl(k[2], Phrases[l,1])){
          
          wds <- isolate(data.frame(stringr::str_extract_all(Phrases[l,1], pattern)))
          
              if (nrow(wds) > 0){
              wds$stat <- paste(k[1], sep=" ")
              
              names(wds) <- names(NewPhrases)
              
              NewPhrases <- rbind(setDT(NewPhrases), setDT(wds), fill = T)
              NewPhrases <- na.omit(NewPhrases)
            }
          }
        }

      }
    }
    
    N <- aggregate(NewPhrases[,1], list(NewPhrases$stat), paste, collapse=",")
    N$x <- sapply(strsplit(N$x, ","), function(i) 
      paste(unique(i), collapse = ","))
    
    colnms <- c("Stat", "Value")
    
    colnames(N) <- colnms
    
    lst <- strsplit(as.character(N$Stat), split = " ")
    
    N$Value <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",N$Value[y])},lst,1:length(lst))

    #Space after comma
    
    N[] <- lapply(N, textclean::add_comma_space)
    N[] <- lapply(N, textclean::add_comma_space)
    
    
    #remove trailing and/or leading commas
    
    N$Value <- trimws(N$Value, whitespace = ",")   
    N$Value <- trimws(N$Value, whitespace = " ")
    N$Value <- trimws(N$Value, whitespace = ",")
    N$Value <- trimws(N$Value, whitespace = " ")
    N$Value <- trimws(N$Value, whitespace = ",")
    
    DT::datatable(N,
                  extensions = c('Buttons', 'FixedColumns'), options = list(scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    initComplete = DT::JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
                    "}"),
                     fixedColumns = list(leftColumns = 2, rightColumns = 0)
                  ))
  })
  
  output$slfkwclass <- DT::renderDataTable({
    
    gr <- corp2()$content
    
    ks <- tolower(keywords())
    
    kws <- ks[1]
    kws <- c(as.character(kws),unlist(strsplit((as.character(kws)), " "))[1]) #separate keywords
    
    kw <- ks[-1] #characteristics to choose
    
    Phrases <- data.frame()
    
    # Find phrases with keywords

    for (x in unique(kws)){

      pattern <- paste0("[^.]*",x,"[^.]*\\.")
      NewPhrases <- data.frame(stringr::str_extract(gr, pattern))
      names(NewPhrases) <- "x"
      Phrases <- rbind(setDT(Phrases), setDT(NewPhrases), fill = T)
      Phrases <- na.omit(Phrases)
    }

    Phrases <- data.frame(lapply(Phrases, as.character), stringsAsFactors=FALSE)

    colnames(Phrases) <- 'Picked Phrases'

    NewPhrases <- data.frame("x"=character(0),"stat"=character(0))
    
    nphrases <- matrix(1:nrow(Phrases))

    k <- c(as.character(kws)[1],unlist(strsplit((as.character(kws[1])), " "))[1], as.character(kw)) #list with all keywords and characteristics

    for (j in unique(k)){

        pattern <- paste0("([^\\s]+\\s){0,0}",j,"(\\s[^\\s]+){0,0}") #pattern to find a in all the phrases 

        for (l in unique(nphrases)){

          if (grepl(k[1], Phrases[l,1]) | grepl(k[2], Phrases[l,1])){

            wds <- isolate(data.frame(stringr::str_extract_all(Phrases[l,1], pattern))) #isolate all words (keywords and charcateristics) found in phrases 

            if (nrow(wds) > 0){
              
              wds$stat <- paste(k[1], sep=" ")

              names(wds) <- names(NewPhrases)

              NewPhrases <- rbind(setDT(NewPhrases), setDT(wds), fill = T)
              
              NewPhrases <- na.omit(NewPhrases)
            }
          }
        }
      }
    

    N <- aggregate(NewPhrases[,1], list(NewPhrases$stat), paste, collapse=",")
    N$x <- sapply(strsplit(N$x, ","), function(i)
      paste(unique(i), collapse = ","))

    colnms <- c("Stat", "Value")

    colnames(N) <- colnms

    lst <- strsplit(as.character(N$Stat), split = " ")

    N$Value <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",N$Value[y])},lst,1:length(lst))

    #Space after comma

    N[] <- lapply(N, textclean::add_comma_space)
    N[] <- lapply(N, textclean::add_comma_space)


    #remove trailing and/or leading commas

    N$Value <- trimws(N$Value, whitespace = ",")
    N$Value <- trimws(N$Value, whitespace = " ")
    N$Value <- trimws(N$Value, whitespace = ",")
    N$Value <- trimws(N$Value, whitespace = " ")
    N$Value <- trimws(N$Value, whitespace = ",")
    
    DT::datatable(N,options = list(
                     initComplete = DT::JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
                     "}")
                  ))
  })
  
  observe({
    
    if(!is.null(dir())){
      
  output$drkwclass <- DT::renderDataTable({
    
    #list of files ending in 'pdf'
    files <- list.files(path=dir(), pattern = "pdf$")
    docs <- list.files(path=dir(), pattern = "docx$")
  
    setwd(dir())
    #files <- lapply(files, function(x) paste)
    #read the pdf files
    georep <- lapply(files, pdf_text)
    georep2 <- lapply(docs, textreadr::read_docx)
    
    zero <- which(lengths(georep2)==0)
    docs <- docs[-zero]
    
    georep2 <- georep2[lapply(georep2,length)>0] #keep lists of length higher than ZERO
    
    georep2 <- do.call(rbind.data.frame, georep2)
    
    if(length(georep2)>0){
    colnames(georep2) <- "Text"
    georep2 <- as.data.frame(do.call(paste, as.data.frame(georep2, stringsAsFactors=FALSE)))
    
    colnames(georep2) <- "Text"
    georep2$doc <- docs
    #georep$doc<-qdap::Trim(clean(georep$doc))
    georep2$doc <- trimws(gsub("\\s+", " ", str_trim(georep2$doc)))
    georep2$Text <- tolower(georep2$Text)
    }
    
    
    georep <- do.call(rbind.data.frame, georep)
    colnames(georep) <- "Text"
    georep <- as.data.frame(do.call(paste, as.data.frame(georep, stringsAsFactors=FALSE)))
    
    if(length(georep)>0){
      
    colnames(georep) <- "Text"
    georep$doc <- files
    #georep$doc<-qdap::Trim(clean(georep$doc))
    georep$doc <- trimws(gsub("\\s+", " ", str_trim(georep$doc)))
    georep$Text <- tolower(georep$Text)
    }
    
    georep <- rbind(georep,georep2)
    
    
    kw <- kw()
    
    keywords=c("Slab","footwall", "gravel", "plantfeed","oversize", "thickness", "texture", "volumes", "frequency", colnames(kw))
    keywords = tolower(keywords)
    Phrases <- data.frame("x"=character(0))
    n <- matrix(1:nrow(georep))
    grfiles <- data.frame()
    
    for (i in n){
      
      gr <- georep[i,]
      
      for (x in keywords){
        colnames(Phrases) <- "x" 
        pattern <- paste0("[^.]*",x,"[^.]*\\.")
        NewPhrases <- data.frame(stringr::str_extract_all(gr[1,1], pattern))
        names(NewPhrases) <- "x"
        Phrases <- rbind(Phrases, NewPhrases, fill = T)
        Phrases <- na.omit(Phrases)
        #rm(NewPhrases)
      }
      for (y in (1:ncol(Phrases))){
        names(Phrases) <- as.character(gr[1,2])
        grfiles <- rowr::cbind.fill(grfiles, Phrases, fill = NA)
        Phrases <- data.frame("x"=character(0))
      }
    }
    
    v <- Filter(function(x)!all(is.na(x)), grfiles)
    
    for(i in 1:ncol(v)){
      v[,i][duplicated(v[,i])] <- NA 
      
    }
    
    v <- data.frame(lapply(v, as.character), stringsAsFactors=FALSE)
    
    #keywords and their descriptives
    NewPhrases <- data.frame("x"=character(0),"stat"=character(0))
    n <- matrix(1:ncol(kw))
    grdfs <- list()
    
    for (m in 1:ncol(v)){
      
      Phrases <- as.data.frame(v[,m])
      nphrases <- matrix(1:nrow(Phrases))
      
      for (i in unique(n)){
        
        #k <- c(unlist(strsplit(colnames(kw)[i]," ")),as.character(kw[,i]))
        k <- c(as.character(colnames(kw)[i]),unlist(strsplit((colnames(kw)[i]), " "))[1], as.character(kw[,i]))
        
        for (j in unique(k)){
          
          pattern <- paste0("([^\\s]+\\s){0,0}",j,"(\\s[^\\s]+){0,0}")
          
          for (l in unique(nphrases)){
            
            if (grepl(k[1], Phrases[l,1]) | grepl(k[2], Phrases[l,1])){
              
              wds <- isolate(data.frame(stringr::str_extract_all(Phrases[l,1], pattern)))
              
              if (nrow(wds) > 0){
                wds$stat <- paste(k[1])
                
                names(wds) <- names(NewPhrases)
                
                NewPhrases <- rbind(setDT(NewPhrases), setDT(wds), fill = T)
                NewPhrases <- na.omit(NewPhrases)
              }
            }
          }
        }
      }
      
      N <-aggregate(NewPhrases[,1], list(NewPhrases$stat), paste, collapse=",")
      
      N$x <- sapply(strsplit(N$x, ","), function(i) 
        paste(unique(i), collapse = " , "))
      
      N[,3:4] <- str_split_fixed(N$Group.1, " ", 2)
      N$x <- str_remove_all(N$x,as.character(N$Group.1))
      N$x <- str_remove_all(N$x,as.character(N$V3))
      N$x <- str_remove_all(N$x,as.character(N$V4))
      
      N$x <- gsub("^,*|(?<=,),|,*$", "", (gsub('\\s+', '',N$x)), perl=T)
      
      N <- N[,1:2]
      cols <- c("stat","Value")
      colnames(N) <- cols
      name <- as.character(colnames(v[m]))
      assign(name, N,.GlobalEnv)
      grdfs[[m]] <- mget(name,.GlobalEnv)
      rm(name)
      NewPhrases <- data.frame("x"=character(0),"stat"=character(0))
    }
    
    
    grdf <- as.data.frame(grdfs[[1]])
    
    for (x in 2:length(grdfs)) {
      colnames(grdf)[1] <- "stat"
      gr <- as.data.frame(grdfs[[x]])
      colnames(gr)[1] <- "stat"
      grdf <- merge(grdf, gr, by = "stat", all= T)
    }
    
    #add Spaces
    grdf[] <- lapply(grdf, textclean::add_comma_space)
    
    
    DT::datatable(grdf,
                  extensions = c('Buttons', 'FixedColumns'), options = list(scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
                      "}"),
                    fixedColumns = list(leftColumns = 2, rightColumns = 0)
                    ))

  })
    }
  })
  
  output$plotword <- renderPlot({
    
    gr <- corp()$content
    
    ud_model <- udpipe_load_model('H:/8. General/App 1.4/english-ewt-ud-2.4-190531.udpipe')
    x <- udpipe_annotate(ud_model, x = gr)
    x <- as.data.frame(x)
    ##1 ---------
    #stats <- subset(x, upos %in% "NOUN")
    #stats <- txt_freq(x = stats$lemma)
    #stats$key <- factor(stats$key, levels = rev(stats$key))
    ##2-----
    ## Collocation (words following one another)
    #stats <- keywords_collocation(x = x, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), ngram_max = 4)
    ## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
    #stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
    ## Co-occurrences: How frequent do words follow one another
    #stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"))
    ## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
    #stats <- cooccurrence(x = x$lemma,relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
    ##3----
    ## Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
    
    stats <- textrank_keywords(x$lemma, 
                               relevant = x$upos %in% c("NOUN", "ADJ"), 
                               ngram_max = 5, sep = " ")
    stats <- subset(stats$keywords, ngram > 1 & freq >= 2)
    
    
    wordcloud(words = stats$keyword, freq = stats$freq)
    
  })
  
  
})

