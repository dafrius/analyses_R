library(tidyverse)


subj_count <- function(x) x %>% group_by(subject) %>% tally() %>% summarise(n=n()) 
  
#### High level Aggregate ####

# This takes every csv file with the "Exp" pattern inside raw_data folder and adds them together.

a=list.files(path = "C:/Users/canoluk/OneDrive - UCL/Experiments/LoHi/Final/Raw Data/High", pattern = "Exp", all.files = TRUE,
             full.names = FALSE, recursive = TRUE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
apath <- "C:/Users/canoluk/OneDrive - UCL/Experiments/LoHi/Final/Raw Data/High/"
a <- paste(apath,a,sep="")


compdata <- c() 
#Putting all subject data in one df

for(i in seq(1,length(a), by =1)){
  currsubj <- read.csv(a[i],stringsAsFactors = FALSE)
  if(colnames(currsubj)[1] == "Ã¯..TrialNumber"){
    currsubj <- currsubj %>% rename(TrialNumber=Ã..TrialNumber)
  }
  if(colnames(currsubj)[1] == "ï..TrialNumber"){
    currsubj <- currsubj %>% rename(TrialNumber=ï..TrialNumber)
  }
  nom<-sub("_.*","",basename(a[i]))
  for(i in 1:length(currsubj)){currsubj$subject <- nom}
  compdata <- rbind(compdata, currsubj)
}

highraw <- compdata %>% mutate(cong=case_when(cond == "ISOD" | cond == "ISOS" ~ "iso",
                                              cond == "DD" | cond == "SS" ~ "cong",
                                              cond == "SD" | cond == "DS" ~ "incong"),
                               context=case_when(cond == "ISOD" | cond == "ISOS" ~ "iso",
                                                 cond == "DD" | cond == "DS" ~ "diff",
                                                 cond == "SD" | cond == "SS" ~ "same"))

save(highraw,file="hi_raw_all.rda", compress="xz")

#### High - Cleaning ####

#load("hi_raw_all.rda")

dfh_cl <- highraw %>% group_by(subject) %>%
  filter(rt > .2, rt < mean(rt,na.rm = TRUE)+2.5*sd(rt, na.rm = TRUE)) %>%
  select(ttno,cond,dissim,ori,rt,acc,subject,cong,context) %>%
  rename(TrialNo=ttno) %>%
  mutate(diff=case_when(dissim==0 & acc ==1 ~ 0,
                        dissim==0 & acc ==0 ~ 1,
                        dissim!=0 & acc ==1 ~ 1,
                        dissim!=0 & acc ==0 ~ 0))

xtab1 <- dfh_cl %>% group_by(subject,context,ori,dissim) %>% count()
xtab2 <- dfh_cl %>% group_by(subject,context,ori, dissim) %>% count() %>% spread(subject,n)

# General overlook of trial counts
xtab1 %>% ggplot(aes(x=dissim,y=n,group=context, fill=context,color=context)) +
  geom_bar(stat="identity")+
  facet_wrap(subject~ori)



save(dfh_cl,file="hi_clean_all.rda", compress="xz")

#### Low level Aggregate ####

# same procedure as the high above

b=list.files(path = "C:/Users/canoluk/OneDrive - UCL/Experiments/LoHi/Final/Raw Data/Low", all.files = FALSE,
             full.names = FALSE, recursive = TRUE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
bpath <- "C:/Users/canoluk/OneDrive - UCL/Experiments/LoHi/Final/Raw Data/Low/"
b <- paste(bpath,b,sep="")

lowdata <- c() 
#Putting all subject data in one df
for(i in seq(1,length(b), by =1)){
  currsubj <- read.csv(b[i],stringsAsFactors = FALSE)
  if(colnames(currsubj)[1] == "?..TrialNumber"){
    currsubj <- currsubj %>% rename(TrialNumber=?..TrialNumber)
  }
  nom<-sub("_.*","",basename(b[i]))
  for(i in 1:length(currsubj)){currsubj$subject <- nom}
  currsubj <- currsubj %>% select(age,gender,subject,TrialNumber,bgori,temp,staircase,targetloc,cond,order,resp,rt,acc,contrast) 
  lowdata <- rbind(lowdata, currsubj)}


lowraw <- lowdata 

save(lowraw,file="lo_raw.rda", compress="xz")

#### Low Cleaning ####

load("lo_raw.rda")

lowdata <- lowraw %>% group_by(subject) %>%
  filter(rt > .2, rt < mean(rt,na.rm = TRUE)+2.5*sd(rt, na.rm = TRUE))

xtab1 <- lowdata %>% group_by(subject,bgori,targetloc) %>% count()
xtab1 %>% ggplot(aes(x=targetloc,y=n,group=bgori, fill=bgori,color=bgori)) +
  geom_bar(stat="identity")+
  facet_wrap(~subject)

dfl_cl<- lowdata

save(dfl_cl,file="lo_clean_all.rda", compress="xz")

#### Putting lo-hi together ####


lows <- dfl_cl %>%
  select(TrialNumber,cond,acc,subject,contrast) %>%
  mutate(cond=case_when(cond==1~"same",
                        cond==2~"diff",
                        cond==3~"iso"),
         task="low") %>%
  rename(tNo=TrialNumber,
         dv=acc,
         iv=contrast)


highs <- dfh_cl %>%
  select(TrialNo,context,diff,subject,dissim,ori)%>%
  rename(tNo=TrialNo,
         cond=context,
         dv=diff,
         iv=dissim,
         task=ori) %>%
  mutate(task=case_when(task==180~"inverted",
                        task==0~"upright"))


d_lohi1234 <- rbind(lows, highs)
save(d_lohi1234, file="d_lohi1234.rda")
