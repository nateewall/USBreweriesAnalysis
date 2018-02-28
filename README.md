# Analysis of US Craft beers Alcohol by Volume (ABV) & Internation Bitterness Unit (IBU)

## Analysis Goal
We will be analyzing data on 2410 US craft beers from 558 different breweries to identify different trends in Alcohol by Volume (ABV) and International Bitterness Units (IBU). In this repository you will find the results of that analysis based on the data, code, and reports outlined below.

## Source Data
The Beers dataset contains a list of 2410 US craft beers and Breweries dataset contains 558 US
breweries. 

The datasets descriptions are as follows.
Beers.csv:
Name: Name of the beer.
Beer ID: Unique identifier of the beer.
ABV: Alcohol by volume of the beer.
IBU: International Bitterness Units of the beer.
Brewery ID: Brewery id associated with the beer.
Style: Style of the beer.
Ounces: Ounces of beer.


Breweries.csv:
Brew ID: Unique identifier of the brewery.
Name: Name of the brewery.
City: City where the brewery is located.
State: U.S. State where the brewery is located.


## Code



-All code below is formatted to be used as an RMD file in
R. 

 

-Reading & Manipulating Data from source

The data for this analysis is stored on GitHub in the
following repository:

https://github.com/nateewall/MSDS6306_CaseStudy1

 

 

-In order to execute the R Markdown code in its entirety,
the following packages must be installed (if not already):

 

RCurl

dplyr

fiftystater

ggplot2

Shiny

 

-To install the packages use the install.package function
for each of the packages above:

install.package('RCurl')

 

 

-We will be using R's RCurl package to read this data
directly from the web.

```{r Download Dataset}

 

-Reference RCurl package to read from web

library(RCurl)

 

- Read Breweries dataset

breweriesURL <- 'https://raw.githubusercontent.com/nateewall/MSDS6306_CaseStudy1/master/Breweries.csv'

Breweries <-read.csv(text=getURL(breweriesURL),
header=T)

 

 

- Read the Beers Dataset

beersURL <-
'https://raw.githubusercontent.com/nateewall/MSDS6306_CaseStudy1/master/Beers.csv'

Beers <-read.csv(text=getURL(beersURL), header=T)

```

 

-Exploring the data

-Once the data is read into R data.frames from the web we
will quickly review the format of the data to make sure all feilds were
captured appropriately.

 

Breweries:

 

```{r Review Data}

- Review the data

str(Breweries)

str(Beers)

```

 

-Our Breweries data.frame has 558 observations with 551
distinct breweries from 51 states and 384 different cities.

 

-Below we will see how many breweries are located in each
of the 51 states in our data:

 

```{r Statewise Breweries}

require(RColorBrewer)

- How many breweries are present in each state?

BreweryStates <- summary(Breweries$State)

-Print in sorted order

sort(BreweryStates, decreasing = T)

barplot(BreweryStates, 

       
xlab="States",

        ylab="Count",

        main =
'Breweries by State',

       
col=brewer.pal(12,"Set3"))

```

 

-Colorado has the most breweries in our data with 47
breweries, followed closely by California's 39. Arkansas has the fewest with
2..

 

-Merge the data sets together to have the geography for
each of the beer.

 

-Below is the code used to merge the two data sets by
Brewery ID and a quick review of top and the bottom of our new data set. We
also join this data to get the full State name by geography

 

```{r Merge Dataset}

mergedBeerBreweries<-merge(

  Breweries, Beers,
by.x = "Brew_ID", by.y ="Brewery_id", all= TRUE

)

 

-lets include some additional state information for later
use

mergedBeerBreweries$State<-trimws(mergedBeerBreweries$State)

 

 

-pull the state names to join with abreviated state name

states <- data.frame(cbind(state.abb,
tolower(state.name)))

 

-rename the vars

names(states) <- c('abrev','statename') 

 

-join them together to have the complete statename

mergedData<-merge(

  mergedBeerBreweries,
states, by.x = "State", by.y ="abrev", all= TRUE

)

 

-fill in missing statename for DC

mergedData$statename[which(mergedData$State=='DC')] <-
"district of columbia"

 

-Assign appropriate name 


names(mergedData)[2]<-"BreweryName"

names(mergedData)[5]<-"BeerName"

 

-print the top 6 records

head(mergedData, 6)

-print the bottom 6 records

tail(mergedData, 6)

```

 

-Once the code above is executed we observe the output
above and see that the data appears to have successfully been merged. 

-However, we see that there are lots of NA's in the IBU
column so we will perform a count of all NA's in our data.

 

```{r Find NAs}

- There are multiple ways to get the NA's

- One method is to use sapply..

sapply(mergedData, function(x) sum(is.na(x)))

```

 

-It appears the most of the columns have 0 NA's with the
exception of 62 NA's for ABV & 1005 NA's for IBU. 

-We are assuming this data was not available for the
beers from this group.

 

-Now we will analyze the Beers by ABV & IBU

 

-First, we take a look at the median alcohol content for
each of the states to determine if there are any major differences.

 

```{r ABV & IBU By State}

-call the dply package using the library function

library(dplyr)

 

- summarize ABV & IBU by state

plotData <- data.frame(

 
mergedData[,c("State","statename","IBU","ABV")]
%>%

   
group_by(State,statename) %>% 

   
summarise_all(funs(median),na.rm = T)

)

plotData <- plotData[order(-plotData$IBU),]

 

- Set margins 

par(mar=c(5, 6, 4 ,2))

 

barplot(plotData$IBU, names.arg = plotData$State, 

       
xlab="States",

       
ylab="Median",

        main = 'IBU
by State',

       
col=brewer.pal(12,"Set3"))

```

 

-Breweies from Maine (ME) have a median IBU of 61ppm
isohumulone, 

-IBU is scaled from 1-100, standard Budweiser has 7.

 

```{r Plot ABV}

- sort the data by ABV as this is all about comparing

plotData <- plotData[order(-plotData$ABV),]

 

- Now plot the data 

barplot(plotData$ABV, names.arg = plotData$State, 

       
xlab="States",

       
ylab="Median",

        main = 'ABV
by State',

        col =
brewer.pal(12,"Set2"))

```

 

-We find that our nation's capital holds the tie for the
highest Median ABV with a state more commonly associated with whiskey,
Kentucky. 

-Both boasting a median ABV of 6.25%. For reference
Budweiser comes in aroun 5% ABV.

 

 

-However, the barplots are a little difficult to read. 

-Thus, we opted to plot these median values on maps in
order to better show the relationship between ABV & IBU. 

-We explored these two values geographically.

 

```{r Geographical Comparision IBU}

-call the fiftystater & ggplot2 packages using the
library function

library(fiftystater)

library(ggplot2)

 

-plotting by statename 

plotData$id <- as.character(plotData$statename)

data("fifty_states")

Total <- merge(fifty_states, plotData,
by="id")

 

p <- ggplot(Total, aes(map_id = id)) + 

  -map points to
the fifty_states shape data

  geom_map(aes(fill
= IBU), map = fifty_states) + 

  expand_limits(x =
fifty_states$long, y = fifty_states$lat) +

  coord_map() +

 
scale_x_continuous(breaks = NULL) + 

 
scale_y_continuous(breaks = NULL) +

  labs(x =
"", y = "") +

 
theme(legend.position = "bottom", 

       
panel.background = element_blank())

p1 <- p + scale_fill_continuous(low =
"palegreen", high = "darkgreen", guide="colorbar")

 

-add border boxes to AK/HI

p2 <- p1 + fifty_states_inset_boxes() 

 

-create title & legend label

p2 + labs(fill = "Median IBU" 

          ,title =
"Median IBU for Each State", x="", y="")

```

 

-This shows some of the states with more bitter beer like
Maine, Florida, West Virginia, and New Mexico. 

-This also shows the states with less bitter beer like
Wisconsin. 

-Now lets see which states move up the ABV Scale.

 

```{r Geographical Comparision ABV}

q <- ggplot(Total, aes(map_id = id)) + 

  - map points to
the fifty_states shape data

  geom_map(aes(fill
= ABV), map = fifty_states) + 

  expand_limits(x =
fifty_states$long, y = fifty_states$lat) +

  coord_map() +

 
scale_x_continuous(breaks = NULL) + 

 
scale_y_continuous(breaks = NULL) +

  labs(x =
"", y = "") +

 
theme(legend.position = "bottom", 

       
panel.background = element_blank())

q1 <- q + scale_fill_continuous(low =
"thistle1", high = "darkred", guide="colorbar")

- add border boxes to AK/HI

q2 <- q1 + fifty_states_inset_boxes() 

q2 + labs(fill = "Median ABV" 

          ,title =
"Median ABV for Each State", x="", y="")

 

```

 

-It is interesting to see certain states favor both High
ABV & High IBU like West Virginia & New Mexico, while the bitterness
factor for Maine falls more towards the middle of ranks in terms of ABV. 

-This may be driven by the state controlled ABV limits.

 

-So, which state has the maximum alcoholic (ABV) beer? 

-And which state has the most bitter (IBU) beer?

 
```{r MaxABV}

-#get the max ABV

maxABV <- mergedData[which(mergedData$ABV ==
max(mergedData$ABV,na.rm = T)), ]

paste0(maxABV$BeerName, " from ", maxABV$City,
", ", maxABV$State, " has the maximum ABV in our data with
", maxABV$ABV, sep=" ")

```

 

-The Lee Hill Series Vol. 5 - Belgian Style Quadrupel Ale
from Colorado has the max alcohol by volumne (ABV) beer. However, we have
several others that deserve honorable mention.

```{r Max ABV}
top10ABV <- mergedData[order(-mergedData$ABV),][1:10,]
top10ABV[,c("BeerName","City","State","ABV")]

```

-Both Colorado & Michigan are representated well in
the top 10 with 5 of the top 10 by ABV.

```{r Max IBU}

-get the max IBU

maxIBU <- mergedData[which(mergedData$IBU ==
max(mergedData$IBU,na.rm = T)), ]

paste0(maxIBU$BeerName, " from ", maxIBU$City,
", ", maxIBU$State, " has the maximum ABV in our data with
", maxIBU$IBU, sep=" ")

```

-Astoria Bitter Bitch Imperial IPA has the max IBU of 138 from Portland, OR. 
-Additionally will we look at the top 10 beers by IBU.

 

```{r Top10 IBU}

top10IBU <- mergedData[order(-mergedData$IBU),][1:10,]

top10IBU[,c("BeerName","City","State","IBU")]

```

 

-The duplicate Heady Topper's may just be an error.
-Or it may just be so good we felt it needed to be in the data twice! Ranked 4.72/5 on Beer Advocate and -2 overall!

 

 

-Now before we look directly at the relationship between
them, lets get an idea of how IBU & ABV are distributed in our data.

 

```{r Shiny Histogram ABV & IBU}

- Summary statistics for the ABV variable.

- summary(mergedBeerBreweries$ABV)

 

-call the shiny package using the library function

library(shiny)

server <- function(input, output) {

  - Define
histogram output

  output$beerHist
<- renderPlot({  -
"beerHist" in UI also

    bc   <- as.numeric(input$var)  - bc = "beer column"

    x    <- mergedData[, bc]

    hist(x, 

         col    = '-8DC13D',  - Green 

         border =
'white',

         main   = paste("Histogram of",
names(mergedBeerBreweries[bc]))

    )

  })

}

 

ui <- fluidPage(

 
titlePanel("Charting Beer Data"),

  sidebarLayout(

    sidebarPanel(

      - Dropdown menu for selecting variable from
beer data.

     
selectInput("var",

                 
label = "Select variable",

                 
choices = c(

                              "ABV" =
7,

                              "IBU" =
8,

                              "Ounce"=10

                              ),

                 
selected = 7)      - Default
selection

    ),

    mainPanel(

     
plotOutput("beerHist")  -
"beerHist" in server also

    )

  )

)

 

shinyApp(ui = ui, server = server)

-hist(mergedBeerBreweries$ABV)

```

-Based on this ABV seems centered around 5.6% with a slight right skew. 

-While IBU's in this dataset appear even more right skewed with a median IBU of 35.

-Now lets explore how these two variables relate.

 

```{r Regression Analysis}

-Is there an apparent relationship between the bitterness
of the beer and its alcoholic content? 

-Draw a scatter plot.

 

plot(mergedData$IBU, mergedData$ABV, 

    
xlab="International Bitterness Units",

    
ylab="Alcohol by volume",

     main =
'Alcohol Volume Vs. Bitterness correlation',

    
col=brewer.pal(2, "Set1"), pch=19)

abline(lm(mergedData$ABV~mergedData$IBU),
col="red")

```

 

Ultimately we conclude that there is a postive trend with
the linear regression plot suggests there is positive correlation between
Alcohol volume and bitterness. However, IBU only explains ~45% of the
variability in ABV alone. Perhaps accounting for geography or beer styles would
help give us more of an understanding of the bitterness in beer. However, w/ over
100 styles in this data that falls outside the scope of this analysis.





## Reports





## References
