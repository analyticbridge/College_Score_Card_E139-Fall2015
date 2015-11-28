## Testing template for project 
##
## 11/28/2015
## Best Bests in Education
## Creates three graphs (tuition, completion rate, and earnings) and
## a table of schools in lowest 25% of tuition and highest 25%
## of completion and earnings and draws them in a 2x2 layout to png.

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

## read  lateset data which is 2013 and merged with 2011 earning data .
edu2013<-read.csv(file = "file2013" , na.strings = "NULL")
## ISSUE:I am having an issue with the first column name not reading in correctly
## compensate by renaminig the col (not optimal but I need a work around).
names(edu2013)[1]<-"UNITID"

## unfortunately, 2011 is latest file with earnings data (md_earn_wne_p6)
## will need to take that data and add it to 2013
edu2011<-read.csv(file = "MERGED2011_PP.csv" , na.strings = "NULL")
names(edu2011)[1]<-"UNITID"
edu2011<-dplyr::select(edu2011, UNITID_2011=UNITID, EARN_2011=md_earn_wne_p6)
edu<-merge(x=edu2013, y=edu2011, by.x="UNITID", by.y="UNITID_2011", 
           x.all=FALSE, y.all=FALSE)

edu<-dplyr::select(edu, 
                   INSTNM,         ## Institution name
                   PREDDEG,        ## Predominate degree
                   CURROPER,       ## Currently operating flag
                   TUITIONFEE_IN,  ## In-state tuition and fees
                   C150_4,         ## Completion rate*
                   EARN_2011)      ## Median earnings** 

## Notes:
## *Completion rate based on 1st-time, full-time students completing in 6 years 
##      (150% of expected time) at 4-year institutions
## **Median earnings based on students working and not enrolled 6 years after entry

edu<-filter(edu, 
            PREDDEG==3 &                        ## Predominate degree is bachelor's
              CURROPER==1 &                       ## Currently operating
              is.na(TUITIONFEE_IN) == FALSE &     ## Key measurements aren't missing
              is.na(C150_4) == FALSE &
              is.na(EARN_2011) == FALSE &
              EARN_2011 != "PrivacySuppressed")

edu$EARN_2011 = as.numeric(as.character(edu$EARN_2011))
edu$INSTNM = as.character(edu$INSTNM)

################### Graph Tuition Cost ###################

## find lowest 25% for in-state tuition
cost_25 <- quantile(edu$TUITIONFEE_IN, .25)
cost_25 <- round(unname(cost_25)) ## unname removes 25% label

## build custom annotation
text_cost <- paste("In-state lowest 25%:\n$", as.character(cost_25), 
                   " or less", sep="")
grob_cost = grobTree(textGrob(text_cost, x=0.50,  y=0.90, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="bold")))

## plot
g_cost <- ggplot(data=edu)
g_cost <- g_cost + geom_density(aes(x=TUITIONFEE_IN), fill = "red", alpha=.25)
g_cost <- g_cost + geom_vline(aes(xintercept = cost_25), linetype="longdash", size=2)
g_cost <- g_cost + ggtitle("In-State Tuition and Fees") + labs(x="", y="")
g_cost <- g_cost + theme(plot.title = element_text(size=20, face="bold", vjust=2))
g_cost <- g_cost + theme(axis.text.x = element_text(size=20, vjust=2))
g_cost <- g_cost + theme(axis.ticks=element_blank(), ## hide y tick marks
                         axis.text.y=element_blank()) 
g_cost <- g_cost + annotation_custom(grob_cost)
##g_cost

################### Graph Earnings ###################

## find highest 25% for earnings
earn_25 <- quantile(edu$EARN_2011, .75)
earn_25 <- round(unname(earn_25)) ## unname removes % label

## build custom annotation
text_earn <- paste("Earnings top 25%:\n$", as.character(earn_25), 
                   " or more", sep="")
grob_earn = grobTree(textGrob(text_earn, x=0.50,  y=0.90, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="bold")))

## plot
g_earn <- ggplot(data=edu)
g_earn <- g_earn + geom_density(aes(x=EARN_2011), fill="green", alpha=.25)
g_earn <- g_earn + geom_vline(aes(xintercept = earn_25), linetype="longdash", size=2)
g_earn <- g_earn + ggtitle("Median Earnings 6 Yrs. After Entry")
g_earn <- g_earn + labs(x="", y="")
g_earn <- g_earn + theme(plot.title = element_text(size=20, face="bold", vjust=2))
g_earn <- g_earn + theme(axis.text.x = element_text(size=20, vjust=2))
g_earn <- g_earn + theme(axis.ticks=element_blank(), 
                         axis.text.y=element_blank()) ## hide y tick marks
g_earn <- g_earn + annotation_custom(grob_earn)
##g_earn

################### Graph Completion Rate ###################

## find highest 25% for completion
rate_25 <- quantile(edu$C150_4, .75)
rate_25 <- unname(rate_25) ## unname removes % label

## build custom annotation
text_rate <- paste("Completion top 25%:\n", as.character(round(rate_25, 2)),
                   " or more", sep="")
grob_rate = grobTree(textGrob(text_rate, x=0.03,  y=0.90, hjust=0,
                              gp=gpar(col="black", fontsize=20, fontface="bold")))

## plot
g_rate <- ggplot(data=edu)
g_rate <- g_rate + geom_density(aes(x=C150_4), fill="blue", alpha=.25)
g_rate <- g_rate + geom_vline(aes(xintercept = rate_25), linetype="longdash", size=2)
g_rate <- g_rate + ggtitle("6 Yrs. Completion Rate") 
g_rate <- g_rate + labs(x="", y="")
g_rate <- g_rate + theme(plot.title = element_text(size=20, face="bold", vjust=2))
g_rate <- g_rate + theme(axis.text.x = element_text(size=20, vjust=2))
g_rate <- g_rate + theme(axis.ticks=element_blank(), 
                         axis.text.y=element_blank()) ## hide y tick marks
g_rate <- g_rate + annotation_custom(grob_rate)
##g_rate

################### Find Schools ###################

## To make cut must be:
## bottom 25% for in-state tution and
## top 25% for completion rate and earnings
schools <- filter(edu, 
                  TUITIONFEE_IN <= cost_25,
                  C150_4 >= rate_25,
                  EARN_2011 >= earn_25)

## sort by name
schools <- arrange(schools, INSTNM)

## give columns betters names for display
schools <- dplyr::select(schools, 
                         Name=INSTNM,
                         Cost=TUITIONFEE_IN, 
                         Comp=C150_4,
                         Earn=EARN_2011)

## round rates
schools$Comp <- round(schools$Comp, 2)

## trim names else table will be too wide
schools$Name <- substring(schools$Name, 1, 48)

## build custom annotations
text_schools <- paste("Schools in Lowest 25% for Tuition and\n",
                      "Top 25% for Completion and Earnings", sep="")
grob_schools <- textGrob(text_schools, gp=gpar(fontsize=20, fontface="bold"))

## increase table fonts for readability
theme_schools <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 1.5)),
  colhead = list(fg_params=list(cex = 1.5)))

## draw table
t_schools <- tableGrob(schools, rows=NULL, theme=theme_schools)
padding <- unit(0.5,"line")
t_schools <- gtable_add_rows(t_schools, 
                             heights = grobHeight(grob_schools) + padding, pos = 0)
t_schools <- gtable_add_grob(t_schools, grob_schools, t=1, l=1, r=ncol(t_schools))
## code below will make sure table is centered and top justified
t_schools$vp<-viewport(x=.5, y=unit(1,"npc")-.5*sum(t_schools$heights))
##grid.newpage()
##grid.draw(t_schools)

################### Layout ###################

## build custom annotations
top <- "Best Bets in Education"
top_grob = textGrob(top, gp=gpar(fontsize=32, 
                                 fontface="bold"))
bottom <- paste("College Scorecard Data (collegescorecard.ed.gov/data) ",
                "2013 data (earnings 2011) for schools with ",
                "predominantly BS degrees", sep="")
bottom_grob = textGrob(bottom, gp=gpar(fontsize=20))

## set graphics device
png("Education3.png", width=1200, height=1200, units="px")

grid.arrange(g_cost, g_rate, g_earn, t_schools, nrow=2, ncol=2, 
             top = top_grob, bottom = bottom_grob)

dev.off()
