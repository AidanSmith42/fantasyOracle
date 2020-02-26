library(stringr)
library(ggplot2)
library(ggthemes)
library(rlist)
library(RCurl)
library(dplyr)
library(DT)
library(data.table)
library(dummies)
library(lpSolve)
library(ranger)
library(XML)


server <- function(input, output, session){
  
  ####SCRAPE DFS SALARIES####
  dat <- getURL('https://www.footballdiehards.com/fantasyfootball/dailygames/FanDuel-Salary-data.cfm')
  
  dat <- readHTMLTable(dat)
  
  dat <- list.clean(dat, fun = is.null, recursive=F)
  
  dat <- as.data.frame(dat)
  
  WK <- as.character(dat$NULL..3[2])
  
  ##SCRAPE FANTASY DATA BY POSITION AND CALCULATE FORECAST
  observeEvent(input$Position, {
    if(input$Position %in% c("RB", "WR", "TE", "FLEX")){
      r <- paste0("https://www.fantasypros.com/nfl/rankings/half-point-ppr-", tolower(input$Position))
    }
    else{
      p <- input$Position
      if(p == "DEF") p <- 'dst'
      r <- paste0("https://www.fantasypros.com/nfl/rankings/", tolower(p))
    }
    withProgress(message="Scraping Data", value= 0,{
      r <- getURL(r)
      
      tables <- readHTMLTable(r)
      
      tables <- list.clean(tables, fun = is.null, recursive=F)
      
      n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
      
      x <- tables[['rank-data']]
      incProgress(.25)
      x <- x[1:39, -2]
      x$V3 <- as.character(x$V3)
      if(input$Position=="DEF"){
        for(i in 1:32){
          st <- gregexpr('\\(', x$V3[[i]])
          st <- as.numeric(st)
          x$V3[i] <- substr(x$V3[[i]], (st+1), (st+3))
        }
      }else{  
        for(i in 1:39){
          s <- strsplit(x$V3[[i]], " ")
          s <- unlist(s)
          Name<- paste0(s[1]," ", s[3], " ", s[4])
          team<- trimws(substr(Name, (nchar(Name)-3), nchar(Name)))
          if(nchar(team) > 3){
            s <- strsplit(team, " ")
            s <- unlist(s)
            x$Team[i] <- s[2]
          }
          else x$Team[i] <- team
          x$V3[i]<- trimws(substr(Name, 1, (nchar(Name)-3)))
          
        }
      }
      
      if(input$Position %in% c("WR", "FLEX")){
        x$V3 <- if_else(x$V3=="Odell Jr.O. Beck", "Odell Beckham Jr.", x$V3)}
      
      if(input$Position=="FLEX"){

        names(x) <- c("Rank", "Player", "Position", "Opponent", "Best", "Worst", "Avg", "SD", "Team") #"Proj"
      }
      else if(input$Position=="DEF")
      {
        x$V3 <- gsub("\\)", "", x$V3)
        names(x) <- c("Rank", "Player", "Opponent", "Best", "Worst", "Avg", "SD")
      }
      else{
        x$V3 <- if_else(x$V3=="Ronald IIR. Jo", "Ronald Jones II", x$V3)
        names(x) <- c("Rank", "Player", "Opponent", "Best", "Worst", "Avg", "SD", "Team")
      }
      if(input$Position %in% c("QB", "DEF", "K", "TE")){ x <- x[1:24,]}
      x$Rank <- as.numeric(as.character(x$Rank))
      x$Avg <- as.numeric(as.character(x$Avg))
      x$Best <- as.numeric(as.character(x$Best))
      x$Worst <- as.numeric(as.character(x$Worst))
      x$Diff <- x$Worst - x$Best
      x$SD <- as.numeric(as.character(x$SD))
     
      txt <- paste0(input$Position, " Rankings")
      set.seed(52)
      

      ##CALCULATE TIERS FOR PLOT

      cluster <- kmeans(x[, "Avg"], centers= sort(kmeans(x[, "Avg"], centers = 5)$centers),nstart = 25)
      x$Tier <- as.factor(cluster$cluster)
      
      if(!(input$Position %in% c('FLEX', 'RB','K','TE', 'DEF')))names(x)<-c( 'Rank', 'Player', 'Opponent', 'Best', 'Worst',  'Avg', 'SD', 'Proj' ,'Team', 'Diff', 'Tier')
       if(input$Position =='FLEX') names(x)<-c( 'Rank', 'Player', 'Position', 'Opponent', 'Best', 'Worst',  'Avg', 'SD' ,'Team', 'Diff', 'Tier')#, 'Proj'
        else if(input$Position != 'DEF') names(x) <- c( 'Rank', 'Player', 'Opponent', 'Best', 'Worst',  'Avg', 'SD', 'Proj' ,'Team', 'Diff', 'Tier')
      if(input$Position =='DEF'){ 
       names(x) <- c("Rank", "Player", "Opponent", "Best", "Worst", "Avg", "SD", "trash", "n", "Tier") 
      }
       x <- x %>% arrange(Avg) %>% mutate(Rank=seq(1,nrow(x)))
      
      g <- ggplot(x, aes(-Rank, Avg))  + 
        xlab("Average Expert Rank") + ylab("Expert Consensus Rank") + 
        theme_igray()  + geom_errorbar(aes(ymax= Avg+SD/2, ymin = Avg - SD/2, width=.1, colour=Tier), alpha=.7, size=.9) +
        scale_color_wsj()+
        geom_point(size=2, colour="black") + coord_flip() + ggtitle(txt) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 30, face = "bold"))
      g <- g + geom_text(aes(label=Player, colour=Tier, y=(Avg-nchar(Player)/5- SD/1.4), size=2.4), show.legend = F) + scale_x_continuous(breaks = NULL)
      if(input$Position=="DEF"){
        tabl <- x %>% select(Rank, Player, Opponent, Avg, Best, Worst, SD)  
      }
      else if(input$Position=="FLEX"){
  
        tabl <- x %>% select(Rank, Position, Player, Team, Opponent, Avg, Best, Worst, SD)  
      }
      else
        tabl <- x %>% select(Rank, Player, Team, Opponent, Avg, Best, Worst, SD)
      
      output$plot <- renderPlot(g)
      if(input$Position%in% c("FLEX", "K", "DEF"))
        output$table <- renderDT(tabl, options=list(pageLength=39, scrollX = '400px', scrollY='400px', dom="ft"))
      updateSelectInput(session, "PlayerName", choices = x$Player)
      
      if(input$Position %in% c("QB", "RB", "WR", "TE")){
        r <- getURL(paste0("https://football.fantasysports.yahoo.com/f1/pointsagainst?season=2019&pos=",input$Position,"&mode=average"))
        
        tables <- readHTMLTable(r)
        tables <- list.clean(tables, fun = is.null, recursive=F)
        
        if(input$Position == "QB"){
          Defense <- tables$statTable0[,c(2,3,7,16)]
          names(Defense) <- c("Team", "PassYds", "RushYds", "PtsAgainst")
          Defense <- Defense %>% select(Team, PtsAgainst, PassYds, RushYds)
        }
        if(input$Position == "RB"){
          Defense <- tables$statTable0[,c(2,6,7,9,10,16)]
          names(Defense) <- c("Team", "Rush", "RushYds", "Rec", "RecYds", "PtsAgainst")
          Defense <- Defense %>% select(Team, PtsAgainst, Rush, RushYds, Rec, RecYds)
        }
        if(input$Position %in% c("WR", "TE"))
        {
          Defense <- tables$statTable0[,c(2,9,10,16)]
          names(Defense) <- c("Team", "Rec", "RecYds", "PtsAgainst")
          Defense <- Defense %>% select(Team, PtsAgainst, Rec, RecYds)
        }
        #Defense <- datatable(Defense, options=list(pageLength=32, scrollY="400px", dom='ft')) %>% formatStyle('PtsAgainst', color=styleInterval(c(18,20), c('red', 'yellow', 'green')))
        Defense$Name <- toupper(substr(Defense$Team, 1, 3))
        for(i in 1:32){
          s <- strsplit(as.character(Defense$Team[i]), "vs")
          s <- unlist(s)
          Defense$TeamName[i]<- trimws(s[1])
          if(Defense$TeamName[i]== "New York Giants") Defense$Name[i] <- "NYG"
          if(Defense$TeamName[i]== "New Orleans Saints") Defense$Name[i] <- "NOR"
          if(Defense$TeamName[i]== "New York Jets") Defense$Name[i] <- "NYJ"
          if(Defense$TeamName[i]== "New England Patriots") Defense$Name[i] <- "NWE"
          if(Defense$TeamName[i]== "Los Angeles Rams") Defense$Name[i] <- "LAR"
          if(Defense$TeamName[i]== "Los Angeles Chargers") Defense$Name[i] <- "LAC"
        }
        #Defense[,"Name"] <- gsub("KAN", "KC", as.matrix(Defense[,"Name"]))
        Defense[,"Name"] <- gsub("JAC", "JAX", as.matrix(Defense[,"Name"]))
        Defense[,"Name"] <- gsub("GRE", "GNB", as.matrix(Defense[,"Name"]))
        Defense[,"Name"] <- gsub("SAN", "SFO", as.matrix(Defense[,"Name"]))
        
        x$Opponent <- as.character(x$Opponent)
        x$Opponent <- trimws(substr(x$Opponent, 4, nchar(x$Opponent)))
        Defense$Opponent <- Defense$Name
        Defense$Opponent <- as.character(Defense$Opponent)
        if(!exists("stats")){
          stats <- fread("Data2.csv",header = T)}
        stats <- as.data.table(stats)
        
        if(input$Position=="RB"){
          stats <- stats[Pos == input$Position | Pos=="HB"]
        }
        else stats <- stats[Pos ==input$Position]
        
        stats2 <- stats[,1:17]
        
        stats2 <- stats2 %>% select(Player, Week, Pos, Tm, Opp, FDPt)
        incProgress(.25)
        
        x2 <- x %>% select(Player, Avg)
        tot <- left_join(stats2, x2)
       
        Defense$Opp <- Defense$Opponent
        tot <- left_join(tot, Defense, by="Opp")
        tot <- tot %>% select(-Team, -TeamName, -Opponent, -Name)
        tot$Player <- as.factor(tot$Player)
        tot$Pos <- as.factor(tot$Pos)
        tot$Tm <- as.factor(tot$Tm)
        tot$Opp <- as.factor(tot$Opp)
        
        NAMES <- names(tot)
        pos <- match("PtsAgainst", NAMES)
        for(i in pos:length(tot)) tot[,i] <- as.numeric(as.character(tot[,i]))
        
        
        
        if(input$Position %in% c("RB", "TE", "WR", "QB")){ 
          names(stats) <- c("Rk", "Player","Pos","Age","Date","Lg","Tm","V8","Opp"  , "Result", "G#" ,    "Week" ,  "Day"  ,  "FantPt",
                            "PPR",    "DKPt"  , "FDPt"  , "Cmp" ,   "PAtt" ,   "PYds" ,   "PTD"  ,   "Int" ,   "RAtt" ,   "RYds" ,   "RTD"  ,   "Rec",    "ReYds",    "ReTD",    
                            "Fmb",    "FGM",    "FGA",    "XPM",    "XPA")  
          stats <- stats %>% select("Player", "Week", "FDPt", "RYds", "RTD", "Rec", "ReYds", "ReTD")
   
          stats$FDPt <- as.numeric(stats$FDPt)
          stats$RYds <- as.numeric(stats$RYds)
          stats$RTD <- as.numeric(stats$RTD)
          stats$Rec <- as.numeric(stats$Rec)
          stats$ReYds <- as.numeric(stats$ReYds)
          stats$ReTD <- as.numeric(stats$ReTD)
          
          stats3 <- stats %>% group_by(Player) %>% summarize(AvgRush= mean(RYds), AvgRuTD = mean(RTD), AvgReYds= mean(ReYds), AvgReTD= mean(ReTD), AvgREC= mean(Rec))
          
          stats$Player <- as.factor(stats$Player)
          stats3$Player <- as.factor(stats3$Player)

          tot <- left_join(tot, stats, by=c("Player", "Week"))
          tot <- left_join(tot, stats3)
          tot <- tot %>% select(-FDPt.y)
          tabl$Team <- if_else(tabl$Player=="Odell Beckham Jr.","CLE",  tabl$Team)
          TABL <- tabl
          TABL$Opponent <- trimws(gsub( "vs.", "", tabl$Opponent))
          TABL$Opponent <- trimws(gsub( "at", "", TABL$Opponent))
          TABL$Opponent <- as.factor(TABL$Opponent)
          TABL$Opp <- TABL$Opponent
          Defense$Opp <- as.factor(Defense$Opp)
          
          TABL[,"Opp"] <- gsub("KC", "KAN", as.matrix(TABL[,"Opp"]))
          TABL[,"Opp"] <- gsub("NO", "NOR", as.matrix(TABL[,"Opp"]))
          TABL[,"Opp"] <- gsub("JAC", "JAX", as.matrix(TABL[,"Opp"]))
          TABL[,"Opp"] <- gsub("GB", "GNB", as.matrix(TABL[,"Opp"]))
          TABL[,"Opp"] <- gsub("SF", "SFO", as.matrix(TABL[,"Opp"]))
          TABL[,"Opp"] <- gsub("NE", "NWE", as.matrix(TABL[,"Opp"]))
          TABL[,"Opp"] <- gsub("TB", "TAM", as.matrix(TABL[,"Opp"]))
        
          TABL$Opp <-as.factor(TABL$Opp)
          HOLDOUT <- inner_join(TABL, Defense, by=c("Opp"))
          tot <- na.omit(tot)
          
          if(input$Position=="RB"){
            
            
            
            HOLDOUT <- HOLDOUT %>% select("Player", "Team.x", "PtsAgainst", "Rush", "RushYds", "Rec", "RecYds")
            names(HOLDOUT)<-c("Player", "Tm", "PtsAgainst", "Rush", "RushYds", "Rec.x", "RecYds")
            HOLDOUT <- left_join(HOLDOUT, stats3)
            
            
            HOLDOUT$PtsAgainst <- as.character(as.numeric(HOLDOUT$PtsAgainst))
            HOLDOUT$Rush <- as.character(as.numeric(HOLDOUT$Rush))
            HOLDOUT$RushYds <- as.character(as.numeric(HOLDOUT$RushYds))
            HOLDOUT$Rec.x <- as.character(as.numeric(HOLDOUT$Rec.x))
            HOLDOUT$RecYds <- as.character(as.numeric(HOLDOUT$RecYds))
            
  
            
            
            #predict rush yards 
            rushingYds = ranger(RYds ~ 
                                  PtsAgainst +
                                  Rush +
                                  RushYds +
                                  Rec.x +
                                  Tm+
                                  RecYds + AvgRush,
                                data = tot,
                                min.node.size = 50,
                                mtry = 3,
                                importance = 'impurity',
                                quantreg = TRUE)
            
            rushingTDs = ranger(RTD ~ 
                                  PtsAgainst +
                                  Rush +
                                  RushYds +
                                  AvgRuTD+ Tm,
                                data = tot,
                                min.node.size = 50,
                                mtry = 3,
                                importance = 'impurity',
                                quantreg = TRUE)
            recTDs =  ranger(ReTD ~ 
                               PtsAgainst +
                               Rec.x +
                               RushYds +
                               RecYds+ 
                               AvgReTD + AvgReYds + AvgREC + Tm,
                             data = tot,
                             min.node.size = 50,
                             mtry = 3,
                             importance = 'impurity',
                             quantreg = TRUE)
            recYds = ranger(ReYds ~ 
                              PtsAgainst +
                              Rush +
                              RushYds +
                              Rec.x +
                              RecYds+ AvgReYds+ Tm,
                            data = tot,
                            min.node.size = 50,
                            mtry = 3,
                            importance = 'impurity',
                            quantreg = TRUE)
            
            REC = ranger(Rec.y ~ 
                           PtsAgainst +
                           Rush +
                           Rec.x +
                           RecYds + AvgReYds + AvgREC+ Tm,
                         data = tot,
                         min.node.size = 50,
                         mtry = 3,
                         importance = 'impurity',
                         quantreg = TRUE)
            
            HOLDOUT <- na.omit(HOLDOUT)
            HOLDOUT$Projections <- (predict(rushingYds, HOLDOUT, type="response")$predictions)*.10 + (predict(rushingTDs, HOLDOUT, type="response")$predictions)*6 +
              (predict(recTDs, HOLDOUT, type="response")$predictions)*6 + (predict(recYds, HOLDOUT, type="response")$predictions)*.10+
              (predict(REC, HOLDOUT, type="response")$predictions)*.5
            
            H2 <- HOLDOUT %>% select(Player, Projections)
            tabl <- inner_join(tabl, H2)
            tabl <- tabl %>% mutate(Projections= round(Projections ,2))  #*(1-(Avg/100))
            tabl <- tabl %>% select(Rank, Player, Team, Opponent, Projections, Avg,  Best, Worst, SD)
          }
          if(input$Position %in% c("WR", "TE")){
            
            
            HOLDOUT <- inner_join(TABL, Defense, by=c("Opp"))
            HOLDOUT <- HOLDOUT %>% select("Player", "Team.x", "PtsAgainst",  "Rec", "RecYds")
            names(HOLDOUT)<-c("Player", "Tm", "PtsAgainst", "Rec.x", "RecYds")
            HOLDOUT <- left_join(HOLDOUT, stats3)
            
            tot <- na.omit(tot)
            recTDs =  ranger(ReTD ~ 
                               PtsAgainst +
                               Rec.x +
                               RecYds+ 
                               AvgReTD + AvgReYds + AvgREC + Tm,
                             data = tot,
                             min.node.size = 50,
                             mtry = 4,
                             importance = 'impurity',
                             quantreg = TRUE)
            recYds = ranger(ReYds ~ 
                              PtsAgainst +
                              Rec.x +
                              RecYds+ AvgReYds + Tm,
                            data = tot,
                            min.node.size = 50,
                            mtry = 3,
                            importance = 'impurity',
                            quantreg = TRUE)
            
            REC = ranger(Rec.y ~ 
                           PtsAgainst +
                           Rec.x +
                           RecYds + AvgReYds + AvgREC + Tm,
                         data = tot,
                         min.node.size = 50,
                         mtry = 3,
                         importance = 'impurity',
                         quantreg = TRUE)
            
            HOLDOUT <- na.omit(HOLDOUT)
            
            HOLDOUT$Projections <- (predict(recTDs, HOLDOUT, type="response")$predictions)*6 + (predict(recYds, HOLDOUT, type="response")$predictions)*.10+
              (predict(REC, HOLDOUT, type="response")$predictions)*.5
            
            H2 <- HOLDOUT %>% select(Player, Projections)
            
            tabl <- inner_join(tabl, H2)
            tabl <- tabl %>% mutate(Projections= round(Projections ,2)) #*(1-(Avg/100))
            tabl <- tabl %>% select(Rank, Player, Team, Opponent, Projections, Avg,  Best, Worst, SD)
          }
          if(input$Position=="QB"){
            if(!exists("qbstats")){
              qbstats <- fread("Qbstats.csv", header=T)}
            qbstats <- as.data.table(qbstats)
            #qbstats <- qbstats[Pos ==input$Position]

            qbstats <- qbstats[,c(2,12,17,20,21,22, 24, 25)]
            names(qbstats) <- c("Player", "Week", "FDPt", "Yds", "TD", "Int", "Rush", "RTD")
            qbstats$FDPt <- as.numeric(qbstats$FDPt)
            qbstats$Yds <- as.numeric(qbstats$Yds)
            qbstats$TD <- as.numeric(qbstats$TD)
            qbstats$Int <- as.numeric(qbstats$Int)
            qbstats$Rush <- as.numeric(qbstats$Rush)
            qbstats$RTD <- as.numeric(qbstats$RTD)
            
            stats3 <- qbstats %>% group_by(Player) %>% summarize(AvgYds= mean(Yds), AvgRuTD = mean(RTD), AvgRush= mean(Rush), AvgTD= mean(TD), AvgInt= mean(Int))
            
            qbstats$Player <- as.factor(qbstats$Player)
            stats3$Player <- as.factor(stats3$Player)
            
            stats3[,"Player"] <- gsub("Mitchell Trubisky", "Mitch Trubisky", as.matrix(stats3[,"Player"]))
            
            tot <- qbstats
          
            #tot <- left_join(tot, qbstats, by=c("Player", "Week"))
            tot <- left_join(tot, stats3)

    
            HOLDOUT <- inner_join(TABL, Defense, by=c("Opp"))
            HOLDOUT <- HOLDOUT %>% select("Player", "Team.x", "PtsAgainst",  "PassYds", "RushYds")
            names(HOLDOUT)<-c("Player", "Tm", "PtsAgainst", "PassYds", "RushYds")
            HOLDOUT <- left_join(HOLDOUT, stats3)
            HOLDOUT <- na.omit(HOLDOUT)
            tot <- na.omit(tot)
            names(HOLDOUT) <- c("Player", "Team", "PtsAgainst", "Yds", 'Rush',  "AvgYds", "AvgRuTD",   "AvgRush",   "AvgTD",     "AvgInt")

            
            rTDs =  ranger(RTD~ 
                             AvgRush +
                             AvgRuTD+ 
                             Rush,
                           data = tot,
                           min.node.size = 50,
                           mtry = 3,
                           importance = 'impurity',
                           quantreg = TRUE)
            rYds = ranger(Rush ~ 
                            AvgRush +
                            Rush,
                          data = tot,
                          min.node.size = 50,
                          mtry = 2,
                          importance = 'impurity',
                          quantreg = TRUE)
            
            Yds = ranger(Yds ~ 
                           AvgYds  + AvgTD,
                         data = tot,
                         min.node.size = 50,
                         mtry = 2,
                         importance = 'impurity',
                         quantreg = TRUE)
            
            TDs =  ranger(TD ~ 
                            AvgYds +
                            AvgTD + Yds,
                          data = tot,
                          min.node.size = 50,
                          mtry = 2,
                          importance = 'impurity',
                          quantreg = TRUE)
            
            
            
            HOLDOUT$Projections <- predict(rTDs, HOLDOUT, type="response")$predictions*6 + predict(rYds, HOLDOUT, type="response")$predictions*.10+
              predict(Yds, HOLDOUT, type="response")$predictions*.04 + predict(TDs, HOLDOUT, type="response")$predictions*4
            
            H2 <- HOLDOUT %>% select(Player, Projections)

            tabl <- inner_join(tabl, H2)
            tabl <- tabl %>% mutate(Projections= round(Projections ,2)) #*(1-(Avg/150))
            tabl <- tabl %>% select(Rank, Player, Team, Opponent, Projections, Avg,  Best, Worst, SD)
            #   
          }
          WK <- 1
          output$Rankings <- renderUI(HTML(paste0("<center> <h3> <b>Week ", WK," " ,  input$Position, " Rankings </h3><b><br>
                                                  <h5> Using projections from the top fantasy experts, the above chart provides a consensus ranking for .5ppr players. <br> The consensus average ranking is plotted, with standard deviations shown in the table below. <br> Additonally, a machine learning model has been used to forecast player projections. </h5></center><br>")))
          output$table <- renderDT(tabl, options=list(pageLength=39, scrollX = '400px', scrollY='400px', dom="ft"))
        }
        ####################
        
        Defense$Rank <- 1:32
        Defense <- Defense %>% select(-Name, -TeamName, -Opponent, -Opp)
        
        Defense <- datatable(Defense, options = list(
          pageLength = 32,
          scrollY="400px",
          dom="ft")) %>% formatStyle('Team', 'Rank',
                                     backgroundColor = styleInterval(c(10, 20), c('LightGreen', 'LemonChiffon', 'pink' )))
        
        output$DefDesc <- renderUI(HTML(paste0("<center> <h3> <b>Average Fantasy Points Given Up To ", input$Position, " By Team </h3><b></center><br>")))
        output$Defense <- renderDT(Defense)
      }
      else {
        output$Defense <- renderDT(NULL)
        output$DefDesc <- renderUI(NULL)
      }
      })
  })
  output$img <- renderImage({
    list(src="2018.gif", contentType="image/gif")
  }, deleteFile=F)
  
  output$giff <- renderUI(HTML(paste0("<h3> <b> <center>2018 Year in Review </h3><b></center><br>")))
  ######BLOG#####
  output$blog <- renderUI(HTML("<h4> The 2018-19 NFL Season was the highest scoring season in history.  With record setting offenses throughout the league,
                               there was a lot to like from a fantasy perspective.  Here's a rundown of the top non-QB performers by week. </h4>"))
  
  output$btcAddy <- renderUI(HTML(paste0("<h3> <b>Bitcoin Address: 38sFv63WrCrEkDWGhmrinoCyVav4HYtsGS </h3><b><br>")))
  
  #dat <- read.csv("dfs.csv")
  ##DO LINEAR PROGRAMMING TO OPTIMIZE ROSTERS
  dat[1,] <- NA
  
  dat <- dat[,c(1,2,5,6,7)]

  names(dat) <- c("Name", "Position", "Salary", "Points", "p2")
  dat$Points <- as.numeric(as.character(dat$Points))
  dat$p2 <- as.numeric(as.character(dat$p2))
  dat <- na.omit(dat)
  
  dat$Salary <- as.character(dat$Salary)
  dat$Salary <- as.numeric(gsub('\\$', "", dat$Salary))
  dat <- dat %>% mutate(Points = Points+p2) %>% select(-p2)
  fd <- dat[order(dat[, "Position"]), ]
  if(nrow(fd) == 0) return(NULL)
  Position.Mat <- dummy(fd[, "Position"])


  if(length(colnames(Position.Mat)) != 5) return(NULL)
  colnames(Position.Mat) <- c("D", "QB", "RB", "TE", "WR")
  
  Position.Mat <- cbind(Position.Mat, Flex = rowSums(Position.Mat[, c("RB", "TE", "WR")]))
  
  f.obj <- fd[, "Points"]
  
  f.con <- t(cbind(Salary = fd[, "Salary"], Position.Mat))
  colnames(f.con) <- fd$Name
  f.dir <- rep(0, nrow(f.con))
  f.rhs <- rep(0, nrow(f.con))
  
  f.dir[1] <- "<="
  f.rhs[1] <- 60000
  
  f.dir[2] <- "="
  f.rhs[2] <- 1
  
  f.dir[3:nrow(f.con)] <- c("=", ">=", ">=", ">=", "=")
  f.rhs[3:nrow(f.con)] <- c(1, 2, 1, 3, 7)
  opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
  picks <- fd[which(opt$solution == 1), ]
  picks <- as.data.table(picks)
  names(picks) <- c("Name", "Position", "Salary", "Projected Points")
  pName <- as.character(picks$Name)
  updateSelectInput(session, 'removeplyr', choices=pName)
  output$Optimizer <- renderUI(HTML(paste0("<center> <h3> <b> Optimal FanDuel Lineup<b><br></center>")))
  output$Picks <- renderDT(picks, options=list(dom="t"))
  
  observeEvent(input$ReOptimize,{

    dat <- dat %>% filter(Name != input$removeplyr)
    fd <- dat[order(dat[, "Position"]), ]
    if(nrow(fd) == 0) return(NULL)
    Position.Mat <- dummy(fd[, "Position"])
    colnames(Position.Mat) <- c("D", "QB", "RB", "TE", "WR")
    
    Position.Mat <- cbind(Position.Mat, Flex = rowSums(Position.Mat[, c("RB", "TE", "WR")]))
    f.obj <- fd[, "Points"]
    
    f.con <- t(cbind(Salary = fd[, "Salary"], Position.Mat))
    colnames(f.con) <- fd$Name
    f.dir <- rep(0, nrow(f.con))
    f.rhs <- rep(0, nrow(f.con))
    
    f.dir[1] <- "<="
    f.rhs[1] <- 60000
    
    f.dir[2] <- "="
    f.rhs[2] <- 1
    
    f.dir[3:nrow(f.con)] <- c("=", ">=", ">=", ">=", "=")
    f.rhs[3:nrow(f.con)] <- c(1, 2, 1, 3, 7)
    
    opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
    picks <- fd[which(opt$solution == 1), ]
    picks <- as.data.table(picks)
    names(picks) <- c("Name", "Position", "Salary", "Projected Points")
    pName <- as.character(picks$Name)
    updateSelectInput(session, 'removeplyr', choices=pName)
    output$Optimizer <- renderUI(HTML(paste0("<center> <h3> <b> Optimal FanDuel Lineup<b><br></center>")))
    output$Picks <- renderDT(picks, options=list(dom="t"))
  })
  
}
