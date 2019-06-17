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

## @knitr set_up_code

isMac <- TRUE
#isMac <- FALSE  ##Set to false if you are NOT running on a Mac.

ifelse(isMac,repos <<- "https://cran.revolutionanalytics.com/" , repos <<- "http://cran.us.r-project.org")

#Load libraries, installing as necessary
if(!require(jsonlite)) install.packages("jsonlite", repos = repos)
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(lubridate)) install.packages("lubridate", repos = repos)
if(!require(R.utils)) install.packages("R.utils", repos = repos)

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


## @knitr define_functions

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
myData <- load_JSON_gzip_file(gzFileName)

#Show & Sanity check the data

## @knitr summary_date_range
myData %>% summarize(newest_sale=max(saledate),oldest_sale=min(saledate))

## @knitr summary_proptype_subtype
#Show "Resale' for "SFR" and "CONDOMINIUM
myData %>%
    filter(transtype == "Resale", proptype %in% c("SFR","CONDOMINIUM")) %>%
    group_by(state,propsubtype,proptype) %>%
    summarize(num_sales=n()) %>%
    select(state,propsubtype,proptype,num_sales) %>%
    xtabs(num_sales ~ proptype + propsubtype, data=.) %>%
    ftable()

myData %>%
    summarize(num_sales=n())

## @knitr cleanse_data

## CLEANSE DATA
#
# Do additional data cleansing here, if needed.
# Possibly look for NA and zeros for bedrooms
# the determine whether to remove, or set to the mean. (if they are outliers, then remove.)

## @knitr summary_by_year
myData %>%
    mutate(year_sold=year(ymd(saledate))) %>%
    group_by(state,proptype,year_sold) %>%
    summarize(num_sales=n()) %>%
    select(state,proptype,year_sold,num_sales) %>%
    xtabs(num_sales ~ proptype + year_sold , data=.) %>%
    ftable()



# set all NA and 0 bedrooms to 1.



# 3-Way Frequency Table
# mytable <- xtabs(~A+B+c, data=mydata)
# ftable(mytable) # print table
# summary(mytable) # chi-square test of indepedence

#run()

##
## TODO
##
#
# Summarize data
#
# Cleanse Data
#
# Build model
#
# Run model
#
# Copy sample  code for models.
#
# Modify sample code.
#
# Look for sample graphics -- put in place holders.
#
# Summarize Data
# Set cache=TRUE for finalized outputs in the Rmd report.
#

##
## Done!
##
#Create new project on git hub
#Create RStudio project
#Copy files into project
#Create Report file.
# TEST git hub links
# Outline report sections
# Test saving and loading files into getwd()
# Import named knitr tags into code chunks
#




