## real_ml_script.R

## RealML - GET REAL ESTATE HOME SALES WITHIN 5 MILES OF MY HOME.

#########################################################
# **HarvardX Data Science Capstone Class**
#     PH125.9x (2T2018)
#
# * Student: Philip Brown
# * email: Phil@pjb3.com
# * github: https://github.com/pjbMit
#########################################################

##############################################################################
# **RealML** - *A Real Estate Machine Learning Project*
#
#     This is R Code script for RealML, a real estate machine learning project and report
#     created by Philip J Brown (pjbMit@pjb3.com)
#     as the final capstone project for the Data Science Certificate program
#     offered by HarvardX: PH125.9x throughh edx.org.
#############################################################################




isMac <- TRUE
#isMac <- FALSE  ##Set to false if you are NOT running on a Mac.

ifelse(isMac,repos <<- "https://cran.revolutionanalytics.com/" , repos <<- "http://cran.us.r-project.org")

#Load libraries, installing as necessary
if(!require(jsonlite)) install.packages("jsonlite", repos = repos)
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(lubridate)) install.packages("lubridate", repos = repos)
if(!require(R.utils)) install.packages("R.utils", repos = repos)
if(!require(caret)) install.packages("caret", repos = repos)
if(!require(corrplot)) install.packages("corrplot", repos = repos)  #provides corrplot visualizarion
if(!require(kernlab)) install.packages("kernlab", repos = repos)  #provides 'rvmLinear' model method
if(!require(knitr)) install.packages("knitr", repos = repos)
if(!require(broom)) install.packages("broom", repos = repos)

# Multicore processing package for caret.
if(!require(doMC)) install.packages("doMC", repos = repos)
registerDoMC(cores=8)


# ATTOMDATA R Code
# Data download from a RESTful API from
# the commercial service ATTOM DATA.COM
#
# Note: The code includes an API key.
# This key has limited usage rights,
# and *will* expire at some point, hopefully
# *after* this project has been reviewed and graded!
#
# The data is download, filtered and then saved to a gzip'd JSON file
# As a result, you can still run this project by downloading the JSON file
# from git hub, even if you can't or don't want to acess the ATTOM DATA API.
#
# getwd() returns the working directory
#
webParam <- data.frame(
    apikey = "736f1130096aa92549d800921bca8e8c",
    lat ="38.839121",
    lon = "-77.098171",
    radius = "5",
    startdt = "2012-01-01",
    enddt = "2019-07-01",
    minPrice = "5000",
    maxPrice = "1000000",
    pageSize = "10",     ## Sets num of records... up to 10000
    downloads = "5",
    stringsAsFactors = FALSE
)
webCmdStr<-"curl -X GET --header 'Accept: application/json' --header 'apikey: %s' --header 'accept: application/json' 'https://api.gateway.attomdata.com/propertyapi/v1.0.0/sale/snapshot?latitude=%s&longitude=%s&radius=%s&orderBy=SaleSearchDate+desc&startsalesearchdate=%s&endsalesearchdate=%s&minSaleAmt=%s&maxSaleAmt=%s&pageSize=%s'"


fileName <- "real_ml_data_file.json"

gzFileName <- paste("real_ml_data_file.json", ".gz",sep="")
## Note: Changed name of gz file loaded, to always load
## the version of the file downloaded from github.
## Comment out the line below
## if you would prefer to load the data that YOUR code
## gets from the ATTOMDATA.com api data service and then saves as a file.
#gzFileName <- paste("real_ml_data_file.github.json", ".gz",sep="")



#Function to download web data, save to gzip file, and return data as a data_frame()
getDataFromWeb <- function(webParam) {
    #Get data from ATTOMDATA.com using apikey
    enddt_for_search <- webParam$enddt
    for(i in seq(1:webParam$downloads)){
        if(!exists("enddt_for_search")) enddt_for_search <- webParam$enddt
        print(paste("enddt_for_search",enddt_for_search))
        cmd <- sprintf(webCmdStr,webParam$apikey,
                       webParam$lat,
                       webParam$lon,
                       webParam$radius,
                       webParam$startdt,
                       enddt_for_search,  ##Note: this parameter's value changes with each iteration
                       webParam$minPrice,
                       webParam$maxPrice,
                       webParam$pageSize)
        json<- system(cmd,intern = TRUE)
        response <- fromJSON(json); print( paste("Response status is: ", response$status$msg))
        f <- (jsonlite::flatten(response$property,recursive = TRUE))

        d <- f %>% transmute(rowId=identifier.attomId,
                             beds=building.rooms.beds,
                             baths=building.rooms.bathstotal,
                             sqft=building.size.universalsize,
                             yearbuilt=summary.yearbuilt,
                             proptype = summary.proptype,
                             addr=address.line1,
                             city= address.locality,
                             state= address.countrySubd,
                             zip = address.postal1,
                             lat =location.latitude,
                             lon =location.longitude,
                             saledate=sale.salesearchdate,
                             propsubtype=summary.propsubtype, # Only want RESIDENTIAL
                             transtype=sale.amount.saletranstype,  #Only want RESALE
                             price = sale.amount.saleamt)

        enddt_for_search <- ymd(min(d$saledate))-1  #Start search 1 day earler
        if(!exists("rawData")) rawData<<-d[0,]   #Initialize myData as data.frame()
        rawData <- rbind(rawData,d) #Add to rawData
    } #end for
    rawData  #Return the downloaded data.
} #end function

#Function to filter data, that also displays info for a "sanity check" on the data.
filterRawData <- function(rawData){

    #Filter to contain only prop types and prop sub types of interest.
    webData <- rawData %>%
        filter(transtype == "Resale", proptype %in% c("SFR","CONDOMINIUM"), propsubtype %in% c("HOUSE","RESIDENTIAL"))

    webData #Return the filtered data_frame.
}

#Function saves webData as JSON in file, using gzip compression
save_as_JSON_gzip <- function(webData,fileName) {
    cat(file=fileName, toJSON(webData,pretty=TRUE))
    gzip(fileName,overwrite=TRUE,remove=TRUE) #creates gzFileName
}

#Function to load data from gzipped JSON file & return data_frame()
load_JSON_gzip_file <- function(gzFileName) {
    myData <- fromJSON(gunzip(gzFileName,remove=FALSE,overwrite=TRUE),flatten = TRUE)

    #Delete uncompressed file let on filesystem as side effect.
    if (file.exists(fileName))  file.remove(fileName)
    myData
}

# Function to download and save data.
download_and_save_web_data <- function(){

    #Download data from ATTOMDATA.com commercial web service.
    rawData <- getDataFromWeb(webParam)

    # Filter to only keep "Resale" for residential condos and single family homes.
    webData <- filterRawData(rawData)

    #save data as JSON in gzip file.
    save_as_JSON_gzip(webData,fileName)

}

## Note: Uncomment and run the line below
## to fetch raw data using the attomdata.com
## RESTful api and then saving the results to a local file.
#download_and_save_web_data()

#Load data from JSON gzip file to data_frame
readInData <- load_JSON_gzip_file(gzFileName)
factorData <- readInData %>% transmute(beds,baths,sqft,yearbuilt,
                                    lat=as.numeric(lat),
                                    lon=as.numeric(lon),
                                    saledate=ymd(saledate),
                                    zip=as.factor(zip),
                                    city=as.factor(city),
                                    state=as.factor(state),
                                    proptype=as.factor(proptype),
                                    propsubtype=as.factor(propsubtype),
                                    addr=as.factor(addr),
                                    transtype=as.factor(transtype),
                                    price
                                    )
#rm(readInData) #free memory



## @knitr after_setup1
print ("Summary of data loaded")
factorData %>% summarize(newest_sale=max(saledate),oldest_sale=min(saledate))

print("show filtered data cross tab by type and subtype --")
print("which indicates a little filtering and cleaning is needed.")
resale_type_subtype <- factorData %>%
    filter(transtype == "Resale", proptype %in% c("SFR","CONDOMINIUM")) %>%
    group_by(state,propsubtype,proptype) %>%
    summarize(num_sales=n()) %>%
    select(state,propsubtype,proptype,num_sales) %>%
    xtabs(num_sales ~ proptype + propsubtype, data=.) %>%
    knitr::kable()
## @knitr after_setup2


## @knitr cleanse_data
#Look for zero bedrooms, and determine their mean sqft.
factorData %>% filter(beds==0) %>% summarize(num=n(),mean(sqft))


#remove zero bedroom errors, and remove unneeded columns.
myData <- factorData %>% filter(beds != 0) %>%
             select(-transtype,-propsubtype)

## @knitr summary_by_year
factorData %>%
    mutate(year_sold=year(ymd(saledate))) %>%
    group_by(state,proptype,year_sold) %>%
    summarize(num_sales=n()) %>%
    select(state,proptype,year_sold,num_sales) %>%
    xtabs(num_sales ~ proptype + year_sold , data=.) %>%
    ftable()
nrow(factorData)


##Limit to all sales since a specified date
salesSince <- "2018-06-01"
length(which(myData$saledate > ymd(salesSince)))
datasubset <- myData[which(myData$saledate > ymd(salesSince)),]
dim(datasubset)  #that's better!

set.seed(2931)
#The log10 is to use binning to see that we get a good represenation in our sample
trainIndx <- createDataPartition(log10(datasubset$price), p = 0.80, list=FALSE)
myTrain <- datasubset[trainIndx,]
myTest <- datasubset[-trainIndx,]

## @knitr summary_head
summary(myTrain)

#summary_attribute_types
sapply(myTrain, class)

#summary
summary(myTrain)

## @knitr summary_histogram
par(mfrow=c(3,3))
cols <- c("beds","baths","sqft","yearbuilt","lat","lon","price")
sapply(cols, function(colname)(hist(myTrain[,colname], main=colname)))


## @knitr summary_histogram2
par(mfrow=c(2,3))
for(i in c(1:6)) {
    plot(density(myTrain[,i]), main=names(myTrain)[i])
}

## @knitr summary_histogram3
par(mfrow=c(2,3))
for(i in c(1:6)) {
    plot(density(myTrain[,i]), main=names(myTrain)[i])
}

## @knitr correlation_plot
correlations <- cor(myTrain[,c("beds","baths","sqft","yearbuilt")])
corrplot(correlations, method="circle")


# remove correlated attributes
# find attributes that are highly correlated

## @knitr attribute_correation_removal
set.seed(2020)
cutoff <- 0.70
correlations <- cor(myTrain[,c("beds","baths","sqft","yearbuilt")])
highlyCorrelated <- findCorrelation(correlations, cutoff=cutoff)
for (value in highlyCorrelated) {
    print(names(myTrain)[value])
}


## @knitr model_run1



# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
model_cols <- c("beds","baths","sqft")
trainX <- myTrain[,model_cols]
trainY <- myTrain[,"price"]

y_hat <- mean(trainY)
y_hat
set.seed(2931)


fit1 <- train(trainX, trainY, method="knn",tunelength = 10,
                  trControl=trainControl)



grid <- expand.grid(.cp=c(0, 0.05, 0.1))

set.seed(2931)
fit2 <- train(trainX, trainY, method="rpart",
                  preProcess =c("center","scale"),
                  tuneLength = 9,
                  tuneGrid=grid,
                  trControl=trainControl)

res<-resamples(list(knn=fit1,rpart=fit2))
summary(res)


# Stochastic Gradient Boosting
set.seed(2931)
fit3 <- train(trainX, trainY, method="gbm",
                 trControl=trainControl, verbose=FALSE)



model_cols2 <- c("beds","baths","sqft","lat","yearbuilt","zip")
trainX2 <- myTrain[,model_cols]
trainY2 <- myTrain[,"price"]

set.seed(2931)
fit4 <- train(trainX2, trainY2, method="knn",tunelength = 10,
               preProc=c("center", "scale"), trControl=trainControl)


set.seed(2931)
fit5 <- train(trainX2, trainY2, method="rpart",
              preProcess =c("center","scale"),
              tuneLength = 9,
              tuneGrid=grid,
              trControl=trainControl)

# Stochastic Gradient Boosting
set.seed(2931)
fit6 <- train(trainX2, trainY2, method="gbm",
              trControl=trainControl, verbose=FALSE)


model_cols3 <- c("beds","sqft","lat","yearbuilt","zip")
trainX3 <- myTrain[,model_cols]
trainY3 <- myTrain[,"price"]

set.seed(2931)
fit7 <- train(trainX3, trainY3, method="knn",tunelength = 10,
              preProc=c("center", "scale"), trControl=trainControl)


set.seed(2931)
fit8 <- train(trainX3, trainY3, method="rpart",
              preProcess =c("center","scale"),
              tuneLength = 9,
              tuneGrid=grid,
              trControl=trainControl)

# Stochastic Gradient Boosting
set.seed(2931)
fit9 <- train(trainX3, trainY3, method="gbm",
              trControl=trainControl, verbose=FALSE)


results <- resamples(list(knn=fit1, rpart=fit2, gbm=fit3, knn_2=fit4, rpart_2=fit5, gbm_2=fit6))
summary(results)

results2<- resamples(list(knn=fit7, rpart=fit8, gbm=fit9))
summary(results2)

## @knitr new_chunk
# Using the training data to predict test data results.
testX <- myTest[,model_cols]
testY <- myTest[,"price"]
p1<-predict(fit8, newdata = testX)
p2<-predict(fit7, newdata = testX)

## @knitr look_at_errors
head(testY,30)

res <- tibble(y=testY,p1=p1,p2=p2)
glimpse(res)
RMSE(p1,testY)
RMSE(p2,testY)
err = p1-testY
histogram(log(err))
## knitr show_log_plot

# e <-tibble(err=err)
# top_n(e,40)

#hist(error/1000)

# p1[2]
# testY[2]
# mu_hat <- mean(myTrain$price)
# mu <- mean(testY)
# c(mu,mu_hat)
#
# scale(myTrain$price)
# sd(myTrain$price)
#
# rmses
# bothModels <- list(
#     knn = knnFit,
#     tree = rpartFit)


