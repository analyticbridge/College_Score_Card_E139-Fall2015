## Education ROI value by earning and tution cost ,Completion rate.
##
## 11/28/2015
#### Creates three graphs (tuition, completion rate, and earnings) and
## a table of schools in lowest 25% of tuition and highest 25%
## of completion and earnings and draws them .

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

## read  lateset data that has earning data which is 2011 .
f<-file.choose()
edu<-read.csv(f, na.strings = "NULL")
attach(edu)
#take out what you need 
names(edu)[1]<-"INSTNM"
names(edu)[8]<-"EARN" 
edu$INSTNM = as.character(edu$INSTNM)

#edu<-dplyr::select(edu, UNITID_2011=UNITID, EARN_2011=md_earn_wne_p6)
edu<-dplyr::select(edu, 
                      INSTNM,         ## Institution name
                      PREDDEG,        ## Predominate degree
                     CURROPER,       ## Currently operating flag
                   TUITIONFEE_IN,  ## In-state tuition and fees
                   TUITIONFEE_OUT,  ## Out -state tution fees
                    COSTT4_A,        ##cost of attendence per year
                    C150_4,         ## Completion rate*
                    EARN, ## mean earnings** 
                    CONTROL,        # public institution=1,pvt2,3
                    UGDS_WHITE,     #Race-white 
                  UGDS_BLACK,
                   UGDS_ASIAN,
                     UGDS_HISP,
                   ADM_RATE,       # admission rate 
                         SAT_AVG,        #avg sat score of student admitted 
                     NPT4_PUB,       # price of program
                      AVGFACSAL)     ## avg_faculty salary
             
write.csv(edu,file="C:\\@goole-drive-backup\\S139_project_data\\CollegeScorecard_Raw_Data\\edu_dave1.csv")
## Notes:
#calculate ROI college matrix first .

ROI<-data.frame(dplyr::select(edu, 
                                  INSTNM,         ## Institution name
                                  PREDDEG,        ## Predominate degree
                                  CURROPER,       ## Currently operating flag
                                  CONTROL,       ##public collge
                                  TUITIONFEE_IN,  ## In-state tuition and fees
                                  COSTT4_A,     ##cost of tution 
                                  C150_4,         ## Completion rate*
                                  EARN))      ## Median earnings** 

##filter to select and run the ROI matrix

ROI<-dplyr::select(edu, 
                   INSTNM,         ## Institution name
                   PREDDEG,        ## Predominate degree
                   CURROPER,       ## Currently operating flag
                   TUITIONFEE_IN,  ## In-state tuition and fees
                   COSTT4_A,       ##cost of tution
                   C150_4,         ## Completion rate*
                   EARN)      ## Median earnings** 

#use subset command easy 
names(ROI)
ROI<-filter(ROI, 
            PREDDEG==3 &    ## Predominate degree is CURROPER==1 
              CONTROL==1 &   ## PUBLIC 
              is.na(COSTT4_A)== FALSE &   ## Key measurements
              is.na(C150_4) == FALSE &
              is.na(TUITIONFEE_IN) == FALSE &
              EARN != "PrivacySuppressed")
names(ROI)
ROI$EARN = as.numeric(ROI$EARN)

##ROI<-subset(ROI,PREDDEG==3 & EARN !="PrivacySuppressed" & CONTROL==1 & CURROPER==1& COSTT4_A)

#final ROI matrix withNA 
write.csv(edu,file="C:\\@goole-drive-backup\\S139_project_data\\CollegeScorecard_Raw_Data\\ROI_Final1.csv")

################### Graph Tuition Cost ###################

## find lowest 25% for in-state tuition
cost_25 <- quantile(ROI$TUITIONFEE_IN, .25,na.rm=T)
cost_25 <- round(unname(cost_25)) ## unname removes 25% label

## build custom annotation
text_cost <- paste("In-state lowest 25%:\n$", as.character(cost_25), 
                   " or less", sep="")
grob_cost = grobTree(textGrob(text_cost, x=0.50,  y=0.90, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="bold")))

## plot
g_cost <- ggplot(data=ROI)
g_cost <- g_cost + geom_density(aes(x=TUITIONFEE_IN), fill = "red", alpha=.25)
g_cost <- g_cost + geom_vline(aes(xintercept = cost_25), linetype="longdash", size=2)
g_cost <- g_cost + ggtitle("In-State Tuition and Fees") + labs(x="", y="")
g_cost <- g_cost + theme(plot.title = element_text(size=20, face="bold", vjust=2))
g_cost <- g_cost + theme(axis.text.x = element_text(size=20, vjust=2))
g_cost <- g_cost + theme(axis.ticks=element_blank(), ## hide y tick marks
                         axis.text.y=element_blank()) 
g_cost <- g_cost + annotation_custom(grob_cost)
g_cost

################### Graph Earnings ###################

## find highest 25% for earnings
earn_25 <- quantile(ROI$EARN, .75)
earn_25 <- round(unname(earn_25)) ## unname removes % label

## build custom annotation
text_earn <- paste("Earnings top 25%:\n$", as.character(earn_25), 
                   " or more", sep="")
grob_earn = grobTree(textGrob(text_earn, x=0.50,  y=0.90, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="bold")))

## plot
g_earn <- ggplot(data=ROI)
g_earn <- g_earn + geom_density(aes(x=EARN), fill="green", alpha=.25)
g_earn <- g_earn + geom_vline(aes(xintercept = earn_25), linetype="longdash", size=2)
g_earn <- g_earn + ggtitle("Median Earnings 6 Yrs. After Entry")
g_earn <- g_earn + labs(x="", y="")
g_earn <- g_earn + theme(plot.title = element_text(size=20, face="bold", vjust=2))
g_earn <- g_earn + theme(axis.text.x = element_text(size=20, vjust=2))
g_earn <- g_earn + theme(axis.ticks=element_blank(), 
                         axis.text.y=element_blank()) ## hide y tick marks
g_earn <- g_earn + annotation_custom(grob_earn)
g_earn

################### Graph Completion Rate ###################

## find highest 25% for compare 
rate_25 <- quantile(ROI$C150_4, .75)
rate_25 <- unname(rate_25) ## unname removes % label

## build custom annotation
text_rate <- paste("Completion top 25%:\n", as.character(round(rate_25, 2)),
                   " or more", sep="")
grob_rate = grobTree(textGrob(text_rate, x=0.03,  y=0.90, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="bold")))

## plot
g_rate <- ggplot(data=ROI)
g_rate <- g_rate + geom_density(aes(x=C150_4), fill="blue", alpha=.25)
g_rate <- g_rate + geom_vline(aes(xintercept = rate_25), linetype="longdash", size=2)
g_rate <- g_rate + ggtitle("6 Yrs. Completion Rate") 
g_rate <- g_rate + labs(x="", y="")
g_rate <- g_rate + theme(plot.title = element_text(size=20, face="bold", vjust=2))
g_rate <- g_rate + theme(axis.text.x = element_text(size=20, vjust=2))
g_rate <- g_rate + theme(axis.ticks=element_blank(), 
                         axis.text.y=element_blank()) ## hide y tick marks
g_rate <- g_rate + annotation_custom(grob_rate)
g_rate

##### try to score the schools and bring top 25###
# value score is generated by high rate of completion,high ration 
#of earn to tution cost -Top 3 varibale affecting
names(schools)
#remove the outlier 
ROI$score<- c((EARN/COSTT4_A)*C150_4)
ROI<-arrange(ROI,score)
ROI<-subset(ROI,ROI$TUITIONFEE_IN>1)
schools<-tail(ROI,n=25,order(schools$score))
schools <- schools[order(-schools$score),] 
write.csv(edu,file="C:\\@goole-drive-backup\\S139_project_data\\CollegeScorecard_Raw_Data\\schools.csv")
write.csv(edu,file="C:\\@goole-drive-backup\\S139_project_data\\CollegeScorecard_Raw_Data\\ROI.csv")
write.csv(edu,file="C:\\@goole-drive-backup\\S139_project_data\\CollegeScorecard_Raw_Data\\EDU.csv")
##done 

################### Regress ROI School list ###################
names(ROI)
# regress on TUITIONFEE_IN" "COSTT4_A" "C150_4" "EARN"  
hist(ROI$score,col="pink")
hist(ROI$EARN,col="blue")
hist(log(ROI$EARN),col="blue")
hist(ROI$COSTT4_A,col="gray")
hist(ROI$TUITIONFEE_IN,col="gray")
hist(ROI$C150_4,col="green")

modave=lm(EARN~(COSTT4_A+TUITIONFEE_IN+C150_4)^2,data=ROI)
summary(modave)
#remove the c150_4 I SNON SIGNIFICANCE 
modave1=lm(EARN~(COSTT4_A+TUITIONFEE_IN)^2,data=ROI)
summary(modave1)
