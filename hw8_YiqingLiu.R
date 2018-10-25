# IS590Data Visualization
# homework8
# author: Yiqing
# let's do it!

# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("stringr")

## Set the working directory
setwd("/Users/yiqingliu/GoogleDrive/IS590DV/hw8")


# load the data
dc = read.csv("dc-wikia-data.csv", header = TRUE,nrows=100)
marvel = read.csv("marvel-wikia-data.csv", header = TRUE,nrows=100)

# data cleaning
dc$SEX=ifelse(dc$SEX=='Female Characters','Female','Male')
dc$ALIVE=ifelse(dc$ALIVE=='Living Characters','alive','dead')
dc$ALIGN=ifelse(dc$ALIGN=='Good Characters','good','bad')
dc$ID=ifelse(dc$ID=='Secret Identity','Secret','Public')
dc = subset(dc, select = -c(urlslug) )
dc = subset(dc, select = -c(page_id) )
marvel$SEX=ifelse(marvel$SEX=='Female Characters','Female','Male')
marvel$ALIVE=ifelse(marvel$ALIVE=='Living Characters','alive','dead')
marvel$ALIGN=ifelse(marvel$ALIGN=='Good Characters','good','bad')
marvel$ID=ifelse(marvel$ID=='Secret Identity','Secret','Public')
marvel = subset(marvel, select = -c(urlslug) )
marvel = subset(marvel, select = -c(page_id) )
colnames(marvel)[colnames(marvel)=="Year"] <- "YEAR"
library(stringr)
dc$first=str_split_fixed(dc$FIRST.APPEARANCE, ",", 2)[,2]
marvel$first=str_split_fixed(marvel$FIRST.APPEARANCE, "-", 2)[,1]
marvel$mon=marvel$first
marvel$first=match(marvel$first,month.abb)
dc$first=substr(dc$first,2,4)
dc$mon=dc$first
dc$first=match(dc$first,month.abb)
library(ggplot2)
#bar
ggplot(dc, aes(EYE)) +
  geom_bar()+ ggtitle("which eye color is more popular?")

g <- ggplot(dc, aes(mon))+ scale_x_discrete(limits=month.abb)+ ggtitle("which month is productive?")
g + geom_bar()
g + geom_bar(aes(fill = SEX))


g <- ggplot(dc, aes(YEAR))
g + geom_bar()
g +geom_bar(aes(fill = SEX), position = position_stack(reverse = TRUE)) +
  coord_flip() + theme(legend.position = "top")+ggtitle("which year is more productive?")

#scattered
g <- ggplot(dc, aes(YEAR,ALIGN,color=SEX)) + geom_point() 
print(g)+ggtitle("Good characters VS Bad characters")

(plot1 <- ggplot(dc, aes(YEAR,APPEARANCES),xlab("NEW RATING TITLE")) 
  + geom_point() 
  + geom_point(data = marvel,color='red')
  +ggtitle("Year and Appearances \n \nred point: marvel black point: dc"))


# Dotplot: Grouped Sorted and Colored
# Sort by mpg, group and color by cylinder 
HairColor <- function(df,title){
  x <- df[order(df$YEAR),] # sort by year
  x$HAIR <- factor(x$HAIR) # it must be a factor
  x$color='black'
  x$color[x$HAIR=='Red Hair'] <- "red"
  x$color[x$HAIR=='Black Hair'] <- "Black"
  x$color[x$HAIR=='Green Hair'] <- "green"	
  x$color[x$HAIR=='Brown Hair'] <- "brown"
  x$color[x$HAIR=='Blond Hair'] <- "orange"
  x$color[x$HAIR=='Strawberry Blond Hair'] <- "pink"
  x$color[x$HAIR=='Auburn Hair'] <- "red"
  x$color[x$HAIR=='Reddish Hair'] <- "red"
  x$color[x$HAIR=='Blue Hair'] <- "blue"
  x$color[x$HAIR=='Purple Hair'] <- "purple"
  x$color[x$HAIR=='Grey Hair'] <- "grey"
  dotchart(x$YEAR,labels=x$name,cex=0.4,groups= x$HAIR,
           main=title,
           xlab="Year", gcolor="black", color=x$color)

}
HairColor(dc,"First Apperance of DC Characters")
HairColor(marvel,"First Apperance of Marvel Characters")



