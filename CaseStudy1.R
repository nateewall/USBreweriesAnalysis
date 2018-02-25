setwd("~/Desktop/My Desktop/SMU/MSDS6306 - Doing Datascience/CaseStudy1/")

# Read Breweries and Beer dataset
Breweries<-read.csv ("Breweries.csv")
Beers<-read.csv("Beers.csv")

# Name loaded into dataframe as Factor 
# Set the data type to character.
Breweries$Name <- as.character(Breweries$Name)
Beers$Name <- as.character(Beers$Name)

# Review the data
str(Breweries)
str(Beers)

summary(Breweries)
summary(Beers)

# Q1: How many breweries are present in each state?
summary(Breweries$State)

# Q2 Merge beer data with the breweries data. 
# Print the first 6 observations and the last six
# observations to check the merged file.

mergedBeerBreweries<-merge(
  Breweries, Beers, by.x = "Brew_ID", by.y ="Brewery_id", all= TRUE
)

# Assign appropriate name  
names(mergedBeerBreweries)[2]<-"BreweryName"
names(mergedBeerBreweries)[5]<-"BeerName"

#print the top 6 records
head(mergedBeerBreweries, 6)
#print the bottom 6 records
tail(mergedBeerBreweries, 6)

# Q3: Report the number of NA's in each column.
# There are multiple ways to get the NA's
# Can go with sapply..
sapply(mergedBeerBreweries, function(x) sum(is.na(x)))

# colSums(is.na(mergedBeerBreweries))
# apply(is.na(mergedBeerBreweries),2,sum)

# VIM package to create visual representation.
require(VIM)
aggr(mergedBeerBreweries)

# Q4: Compute the median alcohol content and international bitterness unit 
# for each state. Plot a bar chart to compare.
require(dplyr)
medianIBU <- data.frame(
  mergedBeerBreweries %>%
    group_by(State) %>% 
    summarise(median = median(IBU, na.rm = T))
)

medianIBU <- medianIBU[order(-medianIBU$median),]

# Set margin for label(y) visibility
par(mar=c(5, 6, 4 ,2))

barplot(medianIBU$median, names.arg = medianIBU$State, 
        xlab="States",
        ylab="Median",
        main = 'IBU by State')

# Q5: Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter
# (IBU) beer?

#get the max ABV
maxABV <- mergedBeerBreweries[which(mergedBeerBreweries$ABV == max(mergedBeerBreweries$ABV,na.rm = T)), ]
maxABV$State

#get the max IBU
maxIBU <- mergedBeerBreweries[which(mergedBeerBreweries$IBU == max(mergedBeerBreweries$IBU,na.rm = T)), ]
maxIBU$State

# Q6: Summary statistics for the ABV variable.

summary(mergedBeerBreweries$ABV)
hist(mergedBeerBreweries$ABV)

# Q7: Is there an apparent relationship between the bitterness 
# of the beer and its alcoholic content? Draw a scatter plot.
mylm<-lm(mergedBeerBreweries$IBU ~ mergedBeerBreweries$ABV)
plot(mergedBeerBreweries$IBU, mergedBeerBreweries$ABV, 
     xlab="International Bitterness Units",
     ylab="Alcohol by volume",
     main = 'Alcohol Volume Vs. Bitterness correlation',
     col=rgb(0,100,0,150,maxColorValue=255), pch=16)
abline(lm(mergedBeerBreweries$ABV~mergedBeerBreweries$IBU), col="red")

summary(mylm)
require(shiny)
server <- function(input, output) {
  # Define histogram output
  output$beerHist <- renderPlot({  # "beerHist" in UI also
    bc   <- as.numeric(input$var)  # bc = "beer column"
    x    <- mergedBeerBreweries[, bc]
#    bins <- seq(min(is.na(x)), 
#                max(is.na(x)), 
#                length.out = input$bins + 1)
    hist(x, 
#         breaks = bins, 
         col    = '#8DC13D',  # Green 
         border = 'white',
         main   = paste("Histogram of", names(mergedBeerBreweries[bc]))
    )
  })
}

ui <- fluidPage(
  titlePanel("Charting Beer Data"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting variable from beer data.
      selectInput("var",
                  label = "Select variable",
                  choices = c(
                              "ABV" = 7,
                              "IBU" = 8,
                              "Ounce"=10
                              ),
                  selected = 7)      # Default selection
      # Slider to set bin width in histogram
#      sliderInput("bins",
#                  "Number of bins",
#                  min   = 1,
#                  max   = 150,
#                  value = 20)
    ),
    mainPanel(
      plotOutput("beerHist")  # "beerHist" in server also
    )
  )
)

shinyApp(ui = ui, server = server)
