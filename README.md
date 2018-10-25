![](https://ws1.sinaimg.cn/large/006tNbRwly1fvh59oez3dj304t04uaap.jpg)
# Homework 8: Knowing R

|Author|Yiqing Liu|
|---|---
|E-mail|yiqing5@:corn:.edu

### Requirements

I want you to use the data you have to investigate the question of who wins Marvel vs. DC. You are allowed to choose, how you determine. Once again if you are unsure about what trends you want to show, talk to me before starting to code. Further, try to adhere to the following principles in your visualization:

- Do not use more dimensions (color, shape, size, etc.) than you need. Having circles appear both larger and darker due to more divisiveness is redundant.
- Convey only relevant information – think of the message your graphic is meant to present; for each piece of information, ask yourself if the graphic would work equally well without!
Make use of R’s functionalities!

****

### data source  
scource: DC characters and Marvel Characters [ ](https://github.com/inp2/UIUC-iSchool-DataViz/tree/master/fall2018/week09/data)  

****

### data preprocessing


##### rename columns
```
dc$SEX=ifelse(dc$SEX=='Female Characters','Female','Male')
dc$ALIVE=ifelse(dc$ALIVE=='Living Characters','alive','dead')
dc$ALIGN=ifelse(dc$ALIGN=='Good Characters','good','bad')
dc$ID=ifelse(dc$ID=='Secret Identity','Secret','Public')
```
##### delete columns
```
dc = subset(dc, select = -c(urlslug) )
dc = subset(dc, select = -c(page_id) )
```

##### uniform

for two dataset, they have different format to represent when these characters appeared for the first time  
  

DC

|FIRST.APPEARANCE|YEAR|
|---|---
|1939, May|1939|


Marvel 

|FIRST.APPEARANCE|Year|
|---|---
|Aug-62|1962|

we need to rename Marvel.Year to Marvel.YEAR  
and create two new columns(String and integer) to stroe month information  
like this: 
DC

|first|mon|YEAR|
|---|---|---
|5|May|1939|


Marvel

|first|mon|YEAR|
|---|---|---
|8|Aug|1962|

Attention: before using match(data,month.abb), confirm data is in correct format. no space or something else.
```
colnames(marvel)[colnames(marvel)=="Year"] <- "YEAR"

library(stringr)
dc$first=str_split_fixed(dc$FIRST.APPEARANCE, ",", 2)[,2]
marvel$first=str_split_fixed(marvel$FIRST.APPEARANCE, "-", 2)[,1]
marvel$mon=marvel$first
marvel$first=match(marvel$first,month.abb)
dc$first=substr(dc$first,2,4)
dc$mon=dc$first
dc$first=match(dc$first,month.abb)
```

### ggplot (Part of Data)
#### bar 
#####  Question: which eye color is more popular? (DC)
here, ggplot knows you only pass into a variable, so y is the count!

```
ggplot(dc, aes(EYE)) +
  geom_bar()+ ggtitle("which eye color is more popular?")
```
<p align="center">
  <img width="460" height="300" src="https://ws4.sinaimg.cn/large/006tNbRwly1fwk81r9nraj30zf19uq5t">
</p>

![](https://ws4.sinaimg.cn/large/006tNbRwly1fwk81r9nraj30zf19uq5t)


#####  Question: which month is productive?? (DC)
pass into two variables, then ggplot knows what to do. Besides, we can add more information...

```
g <- ggplot(dc, aes(mon))+ scale_x_discrete(limits=month.abb)+ ggtitle("which month is productive?")
g + geom_bar()
g + geom_bar(aes(fill = SEX))
```
![](https://ws2.sinaimg.cn/large/006tNbRwly1fwk8245wkkj30zf19uadi.jpg)  

#####  Question: which year is productive? (DC)
try a different layout

```
g <- ggplot(dc, aes(YEAR))
g + geom_bar()
g +geom_bar(aes(fill = SEX), position = position_stack(reverse = TRUE)) +
  coord_flip() + theme(legend.position = "top")+ggtitle("which year is more productive?")
```
![](https://ws4.sinaimg.cn/large/006tNbRwly1fwk827y5lsj30zf19u77l.jpg)  


#### scattered
#####  Question: Good characters VS Bad characters (DC)
the basical point graph only identifies YES or No, if dots are located at the same place, it look weried, so I use another attributes, it makes more sense.

```
g <- ggplot(dc, aes(YEAR,ALIGN,color=SEX)) + geom_point() 
print(g)+ggtitle("Good characters VS Bad characters")
```
![](https://ws3.sinaimg.cn/large/006tNbRwly1fwk82dzcjbj30zf19u42b.jpg) 

#####  Question: Year and Appearances
study the relationship of two variables? use scattered graph.  And use two datasets to make comparison.

```
(plot1 <- ggplot(dc, aes(YEAR,APPEARANCES),xlab("NEW RATING TITLE")) 
  + geom_point() 
  + geom_point(data = marvel,color='red')
  +ggtitle("Year and Appearances \n \nred point: marvel black point: dc"))
```
![](https://ws3.sinaimg.cn/large/006tNbRwly1fwk82kk334j30zf19u0yw.jpg) 

#### Dotplot: Grouped Sorted and Colored
#####  Question: First Apperance of Marvel Characters with each Hair color? 
let's find out their perferences...  
And also, that's why we need to uniform datasets...
```
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
```
![](https://ws4.sinaimg.cn/large/006tNbRwly1fwk82zlc8oj30zf19u4qp.jpg)
![](https://ws4.sinaimg.cn/large/006tNbRwly1fwk82zlc8oj30zf19u4qp.jpg)

****

