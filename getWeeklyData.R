##GATHER WEEKLY SCORES

for(i in 1:6){
  j <- 0
  while(j<200){
  
  P <- paste0("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=2019&year_max=2020&season_start=1&season_end=-1&pos%5B%5D=WR&pos%5B%5D=RB&pos%5B%5D=TE&is_starter=E&game_type=R&career_game_num_min=1&career_game_num_max=400&qb_start_num_min=1&qb_start_num_max=400&game_num_min=0&game_num_max=99&week_num_min=",
              i,"&week_num_max=",i,
              "&c5val=1.0&order_by=fantasy_points_ppr&offset=",j)
  print(P)
  j <- j+100
  
  
  
  r <-getURL(P)
  tables <- readHTMLTable(r)
  tables <- list.clean(tables, fun = is.null, recursive=F)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  x <- tables[[which.max(n.rows)]]
  if(i==1){final <- x}
  else final <- rbind(final,x)
  }
}


final$Rk <- as.numeric(as.character(final$Rk))
final <- na.omit(final)
final$PPR <- as.numeric(as.character(final$PPR))
final$`G#` <- as.numeric(as.character(final$`G#`))

DT <- data.table(final)
s <- DT[Player %in% u]
Q <- data.frame(Player = rep(u,16), Wk=rep(1:16, each=length(u)), Pts2=0)
s <- data.frame("Player"=s[,2],"Wk" = s[,11], "PPR"=s[,15])
Q$Player <- as.character(Q$Player)
s$Player <- as.character(s$Player)
names(s) <- c("Player", "Wk", "PPR")
DT <- full_join(s,Q)

DT[is.na(DT)] <- 0

#s <- data.frame("Player"=DT[,2],"G#" = DT[,11], "PPR"=DT[,15])
DT$Player <- as.factor(DT$Player)
DT <- as.data.table(DT)
DT <- DT[order(Wk)]
DT[, TotalPts := cumsum(PPR), by=c("Player")]

# s <- filter(DT, TotalPts > 240)
# u <- unique(s$Player)
# u <- as.vector(u)
s <- DT
s <- as.data.table(s)
s$Player <- as.character(s$Player)
#s <- s[Player %in% u]
names(s) <- c("Player", "Week", "Pts", "Pts2", "TotalPts")
#s <- filter(s, Week< 16)

s <- s  %>% group_by(Week) %>% mutate(ordering = rank(TotalPts)) %>% ungroup() 
s$Week <- as.integer(s$Week)
s<- read.csv("Data2.csv")
p<-ggplot(s,
          aes(ordering, group = Player,color=Player,fill=Player)) +
  geom_tile(aes(y = TotalPts/2, 
                height = TotalPts,
                width = 0.9), alpha = 0.9) +
  # text on top of bars
  geom_text(aes(y = TotalPts, label = Player), hjust = -0.1) +
  #text in x-axis (requires clip = "off" in coord_cartesian)
  #geom_text(aes(y = 0, label = Player), hjust = 0.1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_color_viridis_d(name="")+
  scale_fill_viridis_d(name="")+
  #xlim(0,400)+
  scale_y_continuous(name ="Total Fantasy Points(2018)",limits=c(0,450))  +
  theme_tufte(14,"Avenir")+
  guides(color=F,fill=F)+
  labs(title='Week: {frame_time}', x = "",y="Total Fantasy Points(2018)") +
  theme(plot.title = element_text(hjust = 1, size = 22),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) + 
  transition_time(Week)+
  ease_aes('cubic-in-out')

animate(p, nframes = 180, width=1300,fps = 10, end_pause = 20) #again, use anim_save(filename) to save
anim_save("2018.gif")
