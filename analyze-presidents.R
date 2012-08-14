#!/usr/bin/env Rscript

# an analysis of the US budget outlays grouped by presidents
# written by Jason Edgecombe
# license: BSD (2 clause)

# US budget downloaded from (convert from XLS to CSV):
# http://www.whitehouse.gov/sites/default/files/omb/budget/fy2013/assets/hist01z3.xls

# Source: Blade President of the United States, licensed under CC-BY
# Other content from Wikipedia, licensed under CC BY-SA
# list of US presidents downloaded from (click on CSV link at the bottom):
# http://www.freebase.com/view/en/president_of_the_united_states/-/government/government_office_or_title/office_holders

#------------------------- begin configuration ---------------------
# who to blame for inaugural years
# if true, the appropriate president is blamed for the inaugural year's budget
blame.incoming.pres=TRUE
blame.outgoing.pres=TRUE
#------------------------- end configuration ---------------------

require(ggplot2)
require(scales)
require(plyr)
require(lubridate)

options(stringsAsFactors=FALSE)

#----------------- read in and process the list of presidents ------------
presidents.in <- read.csv("data/government_position_held.csv")
presidents <- presidents.in[,c(1,10,6)]
rm(presidents.in)
names(presidents) <- c("name","from","to")
presidents <- presidents[order(presidents$from),]
rownames(presidents)<-1:nrow(presidents)

presidents$from=year(as.Date(presidents$from))
presidents$to=year(as.Date(presidents$to))
presidents[nrow(presidents),"to"]=year(as.Date(Sys.time()))

# adjust the presidents term based on blame for the inaugural year's budget
if (blame.incoming.pres==FALSE){
  presidents$from=presidents$from+1
}
if (blame.outgoing.pres==FALSE){
  presidents$to=presidents$to-1
}

# expand the range of each presidential term to a list of years and a name
dframe <- adply(presidents, 1, summarise, year = seq(from, to))[c("name", "year")]

# ------------- read in and process the US yearly budget --------------------

budget.colnames <- c("year", "receipts", "outlays", "surplus", "receipts.2005", "outlays.2005", "surplus.2005", "composite.deflator", "receipts.gdp", "outlays.gdp", "surplus.gdp")
budget.colclasses <- c("character",rep("numeric",10))
budget<-read.csv("data/hist01z3.csv", skip=3, col.names=budget.colnames)

# discard the transition quarter
budget=budget[budget$year!="TQ",]

# convert all columns to numeric
budget=as.data.frame(lapply(budget, function(x) as.numeric(gsub("[, ]","",gsub(" estimate","", x, perl=T), perl=T))))

# compute the percentage differences between each budget year
budget$outlays.diff.2005=c(NA,diff(budget$outlays.2005))
budget$outlays.2005.pct.diff=100*budget$outlays.diff.2005/budget$outlays.2005

#---------- merge the presidents with the budget and compute stats ----------
pres.budget=merge(dframe, budget)

pres.budget.means=ddply(pres.budget[,c("name", "year","outlays.2005.pct.diff")],
  .(name), summarise, year=min(year),
  mean=mean(outlays.2005.pct.diff, na.rm=TRUE))

pres.budget.means$z.score=(pres.budget.means$mean-mean(pres.budget.means$mean))/sd(pres.budget.means$mean)

# re-order by mean
pres.budget.means=pres.budget.means[order(pres.budget.means$mean),]

# prep the data for graphing
pres.budget.means$name=factor(pres.budget.means$name, levels=pres.budget.means$name)
pres.budget.means$label.pos=sapply(pres.budget.means$mean, max, 0)
pres.budget.mean.value=mean(pres.budget.means$mean)

#------------------------------ yummy graphs --------------------------
g <- ggplot(pres.budget.means, aes(x=name, y=mean)) +
  geom_abline(aes(intercept=pres.budget.mean.value, slope=0, colour="red")) +
  geom_bar(stat="identity")+
  geom_text(aes(x=name, y=label.pos, label=round(mean,2), vjust=-0.5, size=5), show_guide=F) +
  ylab("Percent change") +
  annotate("text", x = 2, y = pres.budget.mean.value, colour="red", label = paste("Mean =",round(pres.budget.mean.value,2)), vjust=-0.5) +
  opts(axis.text.x=theme_text(angle=-90, hjust=0), title="Mean percentage yearly change of US Budget Outlays by President\nPercentage of Year 2005 dollars")

if(interactive()) {
  print(g)
} else {
  ggsave("president-budget.png",g,width=11, height=8)
}
