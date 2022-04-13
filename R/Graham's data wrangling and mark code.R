#load packages
require(dplyr)
require(reshape)
require(RMark)
require(ggplot2)
require(ggpubr)
theme_set(theme_classic() + 
            theme(axis.title = element_text(colour = "black", family = "serif",size = 13),
                  axis.text = element_text(colour = "black", family = "serif",size = 12),
                  legend.title = element_text(family = "serif", colour = "black", size = 12), 
                  legend.text = element_text(family = "serif", colour = "black", size = 12),
                  legend.position = "top",
                  plot.title = element_text(family = "serif", colour = "black", size = 14,hjust = 0.5)))


rm(list = ls())
setwd("C:/Users/ShoupLabUser/OneDrive - Oklahoma A and M System/Graham Montague 2019/graham/Graham Montague 2019/Field Work/FINAL DATASHEET")


############################################ Blackwell #####################################################
blackwell <-readxl::read_excel("Data/Final datasheet.xlsx", 
                        sheet = "Blackwell", col_types = c("date", 
                                                           "text", "text", "numeric" , "numeric", "text", 
                                                           "numeric", "date", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "text", "numeric", "date", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "numeric", "text", "numeric", 
                                                           "text", "numeric", "numeric", "text", 
                                                           "numeric", "numeric", "numeric", 
                                                           "text", "text", "date", "text", "text", 
                                                           "numeric", "numeric"))

warnings()

### need to create a "." for the missing months that we did not sample
### sampled 2019: May, June, July, . (missing-Aug), September, October, November (caught nothing but sampled 0), . (missing-Dec), 
### sampled 2020: . (missing-Jan), . (missing-Feb), March (caught nothing 0), April, May, June, July, August, September, October, November (caught nothing 0), December (caught nothing 0),
### Sampled 2021: January (caught nothing 0), February (caught nothing 0) 
### So ch should look like 11.110...011111110 or 18 characters 
### Need to make fake data for when we sampled but caught nothing

### Create tag data from Mark Book#######

bw.ch <- blackwell %>% 
  group_by(tag, month) %>%
  summarize(tag, .groups = "drop_last")

bw.ch <- bw.ch[!is.na(bw.ch$tag), ]
  
transform = melt(bw.ch, id.vars = "tag")
bw.pivot <- cast(transform, tag ~ value)
bw.pivot[is.na(bw.pivot)]=0

bw.pivot[,2:ncol(bw.pivot)][bw.pivot[,2:ncol(bw.pivot)] != 0]= 1
Lh <- max(bw.ch$month)-min(bw.ch$month)+2

occstr <- seq(min(bw.ch$month), max(bw.ch$month), 1);
occstr <- as.character(occstr);
occstr <- c("tag", occstr);

missing <- setdiff(occstr, names(bw.pivot))
bw.pivot[missing] <- "0"  ##### This is when you can classify the missing months as zeros. It would be nice to put dots where we did not sample at all. 
bw.pivot <- bw.pivot[occstr]

bw.pivot$ch <- apply(bw.pivot[2:Lh],1,paste,collapse="")
bw.pivot[2:Lh] <- NULL

bw.pivot$tag <- paste("/*", bw.pivot$tag, "*/", sep = "")
bw.pivot <- bw.pivot[order(bw.pivot$ch, decreasing=TRUE),]

bw.pivot$end <- "1;" ;
table <- write.table (bw.pivot, file = "cjs-pivot.inp", sep = " ", quote = F, col.names = F, row.names = F);
as.data.frame(table)

### need to delete fake data
### find which row the fake data is under
which(bw.pivot == "/*fake*/", arr.ind=TRUE)

### delete the fake row from the data 
bw.pivot <- bw.pivot[-c(1), ]

### Need to get TL and Temp as covariates??? 
###group.df<-as.data.frame(with(data,table(tag,TL)))
###group.df<-subset(group.df,Freq>0,select=c(TL))

#########################Cormak-Jolly-Seber Model##################################################################
#Write CJS possible parameter estimates

phi.dot=list(formula = ~1) ###Constant Models
p.dot=list(formula = ~1) ###Constant Models
phi.time=list(formula = ~time) ###### Time varying apparent survival 
p.time = list(formula = ~time) ###### Time varying capture probability
p.length=list(formula = ~ TL) #### Capture probability varies by length
p.temp=list(forumla = ~ Temp) #### Capture probability varies by Temperature
p.length.temp= list(formula = ~TL*Temp) #### Capture probability varies by the interaction of Temp and Time

#Set up capture history data for mark, define model########################################

bw.tag.process<-process.data(bw.pivot,model = "CJS",time.intervals = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
tag.ddl <- make.design.data(bw.tag.process)

#constant apparent survival (PHI), constant capture probability (p)
constant <- mark(bw.tag.process,tag.ddl,model.parameters = list(Phi=phi.dot,p=p.dot))

#Time varying capture probability, constant apparent survival
time.p <- mark(bw.tag.process,tag.ddl,model.parameters = list(Phi=phi.dot,p=p.time))

#Length varying capture probability
length.p <- mark(bw.tag.process,tag.ddl,model.parameters = list(Phi=phi.dot,p=p.length))

#Temp varying capture probability
temp.p <- mark(bw.tag.process,tag.ddl,model.parameters = list(Phi=phi.length,p=p.length))

#Compare models
collect.models(type = "CJS")







##### Not complete, wating for feedback from Lake Carl Blackwell from Anna ######
############################### Lake McMurtry #################################################################

mcm <-readxl::read_excel("Final datasheet.xlsx", 
                               sheet = "McMurtry", col_types = c("date", 
                                                                  "text", "text", "numeric", "text", 
                                                                  "numeric", "date", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "text", "numeric", "date", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "numeric", "text", "numeric", 
                                                                  "text", "numeric", "numeric", "text", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "text", "text", "date", "text", "text", 
                                                                  "numeric", "numeric"))

warnings()


### need to create a "." for the missing months that we did not sample
### sampled 2019: June, July, . (missing-Aug), September, October, November (caught nothing 0), . (Dec), 
### sampled 2020: . (Jan), . (Feb), March (caught nothing 0), April, May, June, July, August, September, October, November (caught nothing 0), December (caught nothing 0) 
### Sampled 2021: January (caught nothing 0), February (caught nothing 0)
### So ch should look like 11.110...11111111100 or 18 characters 
### Need to make fake data for when we sampled but caught nothing

### Create tag data from Mark Book#######

#### Need to format to include just month and tags that were caught that month ######

bw.eh <- blackwell %>% 
  group_by(tag, month) %>%
  summarize(tag, .groups = "drop_last")

bw.eh <- bw.eh[!is.na(bw.eh$tag), ]

transform = melt(bw.eh, id.vars = "tag")
pivot <- cast(transform, tag ~ value)
pivot[is.na(pivot)]=0

pivot[,2:ncol(pivot)][pivot[,2:ncol(pivot)] != 0]= 1
Lh <- max(bw.eh$month)-min(bw.eh$month)+2

occstr <- seq(min(bw.eh$month), max(bw.eh$month), 1);
occstr <- as.character(occstr);
occstr <- c("tag", occstr);

missing <- setdiff(occstr, names(pivot))
pivot[missing] <- "0"  ##### This is when you can classify the missing months as zeros. It would be nice to put dots where we did not sample at all. 
pivot <- pivot[occstr]

pivot$eh <- apply(pivot[2:Lh],1,paste,collapse="")
pivot[2:Lh] <- NULL

pivot$tag <- paste("/*", pivot$tag, "*/", sep = "")
pivot <- pivot[order(pivot$eh, decreasing=TRUE),]

pivot$end <- "1;" ;
table <- write.table (pivot, file = "cjs-pivot.inp", sep = " ", quote = F, col.names = F, row.names = F);
as.data.frame(table)

### need to delete fake data
### find which row the fake data is under
which(pivot == "/*fake*/", arr.ind=TRUE)

### delete the fake row from the data 
pivot <- pivot[-c(981), ]

### Need to get TL 
group.df<-as.data.frame(with(data,table(tag,TL)))
group.df<-subset(group.df,Freq>0,select=c(TL))




################################ Boomer Lake ################################################################

boomer <-readxl::read_excel("Final datasheet.xlsx", 
                         sheet = "Boomer", col_types = c("date", 
                                                            "text", "text", "numeric", "text", 
                                                            "numeric", "date", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "numeric", "date", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "numeric", "text", "numeric", 
                                                            "text", "numeric", "numeric", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "text", "text", "date", "text", "text", 
                                                            "numeric", "numeric"))

warnings()





