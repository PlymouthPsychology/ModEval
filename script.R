## plotting psychology lecturer module evaluations
## Jon May
## GPL 3.0


library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(psych)

# ensure working directory set to the same folder as the data
ratings <- read_csv("ME data Lecturing 2017-18 redacted.csv")
## NB this data has redacted staffid and moduletitle, and deleted text comments.
## NB module codes have been rpelaced by random values
## staff names replaced by Person 001 to Person 999

## some code below here assumes original unredacted file is being used and will have no effect on
## the redacted data

#need to correct some staff names so people only have one version of their name
ratings$staff<-gsub("Dr ","",ratings$staff)
ratings$staff<-gsub("Prof ","",ratings$staff)
ratings$staff<-gsub("Christopher ","Chris ",ratings$staff)
ratings$staff<-gsub("William ","Bill ",ratings$staff)



attach(ratings)
# 0 in ratings are 'not applicable' so treat as missing values
explaining<-na_if(explaining,0)
interesting<-na_if(interesting,0)
overall<-na_if(overall,0)

# phil was incorrectly incuded in PSYC108 so drop those rows
ratings <- ratings %>% filter(!(module=='PSYC108PP' & staff == 'Phil Gee'))
## this does nothing if redacted verison used


# find means and SD for each tutor and count responses
tutors <- ratings %>% group_by(staff) %>% 
  summarize(explain.M = mean(explaining, na.rm = TRUE),
            interesting.M = mean(interesting, na.rm = TRUE),
            overall.M = mean(overall, na.rm = TRUE),
            explain.SD = sd(explaining, na.rm = TRUE),
            interesting.SD = sd(interesting, na.rm = TRUE),
            overall.SD = sd(overall, na.rm = TRUE),
            ratingN = n(),
            # use the first letters of the 'FIRST LAST' format names as initials in case it helps with plot
            initials = paste(substr(strsplit(staff," ")[[1]][1],1,1),substr(strsplit(staff," ")[[1]][2],1,1), sep=''))

## build a list of staff we want to plot if using unredacted data
stafftoplot <- c("Alastair Smith","Alison Bacon","Allegra Cattani","Alyson Norman",
                 "Andy Wills","Bill Simpson","Caroline Floccia","Chris Berry","Chris Longmore",
                 "Chris Mitchell","Clare Walsh","Cordet Smart","Ed Symes","Giorgio Ganis",
                 "Haline Schendan","Jackie Andrade","Jeremy Goslin","Jon May","Judy Edworthy",
                 "Laurence White","Marina Wimmer","Matt Roser","Michael Hyland",
                 "Michael Verde","Michaela Gummerum","Patric Bach",
                 "Peter Jones","Phil Gee","Sabine Pahl",
                 "Stephen Hall","Sue Denham","Sylvia Terbeck","Tim Auburn",
                 "Tim Hollins","Yaniv Hanoch"
                  )

## only retain selected staff in data frame if used unredacted data 
#tutors <- tutors %>% filter(staff %in% stafftoplot)

### plot with explaining on y, interesting on x, overall as red to green, and N as size
ggplot(tutors, 
       aes(x=explain.M, y=interesting.M, size=ratingN)) +
  geom_point(aes(color = overall.M)) +
  # use geom_text_repel to prevent names overlapping
  geom_text_repel(aes(label=staff, color = overall.M),hjust=0, vjust=0)+
  scale_color_gradient(low="red", high="green", name="Overall") +
  ylab("Explaining") + xlab("Interesting") 

# correlations between the mean ratings
corr.test(tutors[,2:4], ci=TRUE)

# correlations between the raw ratings
corr.test(ratings[,8:10], ci=TRUE)
