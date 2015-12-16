## project S139 work in progress 
## Variables extraction module you can use.
## load follwoing packages my R ver is 3.1.2
## packages
require(dplyr)
require(ggplot2)
require(grid)
require(gridExtra)
require(gtable)

################### Clean data ###################

## data comes from https://collegescorecard.ed.gov/data/
## select columns of interest and filter data
## make sure variables are correct data type
## BELOW TAKING TWO FILES AND MERGING TOGETHER AND EXTRACTING VARIABLES.

edu2013<-read.csv(file = "C:\\S139projectdata\\MERGED2013_PP.csv" , na.strings = "NULL")
names(edu2013)[1]<-"UNITID"
names(edu2011)[1]<-"UNITID"
#Extract the earning data from 2011 
edu2011<-read.csv(file = "C:\\S139projectdata\\MERGED2011_PP.csv" , na.strings = "NULL")
edu2011<-dplyr::select(edu2011, UNITID_2011=UNITID, EARN_2011=md_earn_wne_p6)
#final merge with 2013
edu<-merge(x=edu2013, y=edu2011, by.x="UNITID", by.y="UNITID_2011", 
           x.all=FALSE, y.all=FALSE)

edu<-dplyr::select(edu, 
                   INSTNM,         ## Institution name
                   PREDDEG,        ## Predominate degree
                   CURROPER,       ## Currently operating flag
                   TUITIONFEE_IN,  ## In-state tuition and fees
                   C150_4,         ## Completion rate*
                   EARN_2011)      ## Median earnings** 
edu<-na.omit(edu)
write.csv(edu,file="C:\\S139projectdata\\edu.csv")
## Let us Filter the data to create clean file 
edu<-filter(edu, 
            PREDDEG==3 &                        ## Predominate degree is CURROPER==1 &                       ## Currently operating
              is.na(TUITIONFEE_IN) == FALSE &     ## Key measurements aren't missing
              is.na(C150_4) == FALSE &
              is.na(EARN_2011) == FALSE &
              EARN_2011 != "PrivacySuppressed")

edu$EARN_2011 = as.numeric(as.character(edu$EARN_2011))
edu$INSTNM = as.character(edu$INSTNM)
#run a model 
fit<-lm(edu$EARN_2011~TUITIONFEE_IN,data=edu)
summary(fit)

## find lowest 25% for in-state tuition
cost_25 <- quantile(edu$TUITIONFEE_IN, .25)
cost_25 <- round(unname(cost_25)) ## unname removes 25% label
## find highest 25% for earnings
earn_25 <- quantile(edu$EARN_2011, .75)
earn_25 <- round(unname(earn_25)) ## unname removes % label
## find highest 25% for completion
rate_25 <- quantile(edu$C150_4, .75)
rate_25 <- unname(rate_25) ## unname removes % label

## To Score the high college must be :
## bottom 25% for in-state tution and ($9162)
## top 25% for completion rate and earnings(64.03%,$37000)
schools <- filter(edu, 
                  TUITIONFEE_IN <= cost_25,
                  C150_4 >= rate_25,
                  EARN_2011 >= earn_25 )
write.csv(schools,file="C:\\S139projectdata\\schools.csv")
#or use subset command 
schools<-subset(edu,TUITIONFEE_IN <= cost_25 & C150_4 >= rate_25 & EARN_2011 >= earn_25)
## sort by name
schools <- arrange(schools, INSTNM)
#run a lm model on  most value education on cost vs tution.
sch<-lm(schools$EARN_2011~TUITIONFEE_IN,data=schools)
summary(sch)
plot(sch)
## creating score variable for edu data
#score = ratio of earn/cost *completion
attach(edu)
> ratio<-EARN_2011/TUITIONFEE_IN
> edu$score<-ratio/C150_4
#try transform and run a model for edu 
