###Problem 1: Statistics 
setwd("C:/Users/annet/OneDrive/Desktop/Linkedin/Ex_Files_Code_Clinic_R/Ex_Files_Code_Clinic_R/Exercise Files/2018_1_summarizeTheWeather/resources")

###Problem Statement 

#write a function that accepts a beginning date and time and
#an ending date and time to return the coefficent of the slope
#of the barometric pressure

####Soltuion 

library(magrittr)
library(lubridate)

#let us import the source data 
mytempfile <- tempfile()
readOneFile <- function(dataPath) {
  read.table(dataPath,
             header = TRUE,
             stringsAsFactors = FALSE)
}
myProgressBar <- txtProgressBar(min = 2012, max = 2015, style = 3) #to ensure your program is not frozen and style 3 means to use it in percent style 
for (dataYear in 2012:2015) {
  dataPath <-
    paste0(
      "https://raw.githubusercontent.com/lyndadotcom/LPO_weatherdata/master/Environmental_Data_Deep_Moor_",
      dataYear,
      ".txt"
    )
  
  if (exists("LPO_weather_data")) {
    mytempfile <- readOneFile(dataPath)
    LPO_weather_data <- rbind(LPO_weather_data, mytempfile)
  } else {
    LPO_weather_data <- readOneFile(dataPath)
  }
  setTxtProgressBar(myProgressBar, value = dataYear)
  
}

#Now wee need to confirm the results of the import
head(LPO_weather_data, n = 3) #gives top 6 lines
tail(LPO_weather_data, n = 3) #gives the last 6 lines
print(paste("Number of rows imported: ", nrow(LPO_weather_data)))

# Calculate the Coefficient of Barometric Pressure
startDateTime <- "2014-01-02 12:03:34"
endDateTime <- "2014-01-04 12:03:34"

# helper function to get a subset of LPO_weather_data
getBaromPressures <- function(dateTimeInterval) {
  subset(
    LPO_weather_data,
    ymd_hms(paste(date, time)) %within% dateTimeInterval, #from lubridate easy way to work with dates and times 
    select = c(Barometric_Press, date, time)
  )
}
calculateBaroPress <- function(startDateTime, endDateTime) {
  dateTimeInterval <- interval(ymd_hms(startDateTime),
                               ymd_hms(endDateTime))
  
  baroPress <- getBaromPressures(dateTimeInterval)
  
  slope <- ymd_hms(paste(baroPress$date, baroPress$time))
  
  lm(Barometric_Press ~ slope, data = baroPress)
  
}
calculateBaroPress(startDateTime, endDateTime)

# Graph Barometric Pressure 
graphBaroPressure <- function(startDateTime, endDateTime ) {
  
  dateTimeInterval <- interval(ymd_hms(startDateTime),
                               ymd_hms(endDateTime))
  
  baroPress <- getBaromPressures(dateTimeInterval)
  
  thisDateTime <- ymd_hms(paste(baroPress$date, baroPress$time))
  
  plot(
    x = thisDateTime,
    y = baroPress$Barometric_Press,
    xlab = "Date and Time",
    ylab = "Barometric Pressure",
    main = paste(
      "Barometric Pressure from ",
      ymd_hms(startDateTime),
      "to",
      ymd_hms(endDateTime)
    )
  )
  abline(calculateBaroPress(startDateTime, endDateTime), col = "red")
}
graphBaroPressure(startDateTime, endDateTime)


###Problem 2: Where am I?
#for geolocation applications 

###Problem Statement 
#should show the geolocation of where you are running your app from
#should show the longitude, lattitude and location as well as an indicator
#of how well accurate the location is

###Solution
library(rjson) #to read json files 
library(maps)
library(mapdata)

#get the geolocation
myIPaddress <- readLines("http://ipv4bot.whatismyipaddress.com/", warn = FALSE)
getGeoString <- paste0("https://ipinfo.io/", myIPaddress,"/json") #to convert IP into location
myLocation <- fromJSON(file = getGeoString)
latlong <- strsplit(myLocation$loc, ",")
myLongitude <- as.numeric(latlong[[1]][2])
myLatitude <- as.numeric(unlist(latlong)[1]) #will return the first element 


#now we need to map the location

maprange = 5
map("worldHires", #which database to use
    xlim = c(myLongitude-maprange, myLongitude+maprange),
    ylim = c(myLatitude-maprange, myLatitude+maprange),
    col = "gray80", fill = TRUE,
    mar = c(1.1, 1.1, par("mar")[3], 2))
points(myLongitude, myLatitude, col="red", pch = 8, cex = 1)
title(paste(myLocation$city, ", ", myLocation$region, " - ", myLocation$country))
map.cities( label = TRUE, minpop = 500000)

###Problem 3: The eight queens

###Problem Statement 
#place the eight queens in such a manner that none
#can attach each other (There are 92 sol)

###Solution
setwd("C:/Users/annet/OneDrive/Desktop/Linkedin/Mastering R For data science/Ex_Files_Code_Clinic_R/Ex_Files_Code_Clinic_R/Exercise Files2018_3_eight_queens")
source("plotTheQueens.R")
library(combinat)

# test values -------------------------------------------------------------
fail_posDiagonal <- c(1,2,3,4,5,6,7,8) # fails because of positive diagonals
fail_negDiagonal <- c(8,7,6,5,4,3,2,1) # fails because of negative diagonals
fail_dupRows <- c(1,1,2,3,4,5,6,7) # fails because of duplicate rows
validSolution <- c(7,5,3,1,6,8,2,4) # This should work (first col of the chessboard)
#scroll down to understand how the col and row conflict is solved (vectors for col and permn() for col)

# Find diagonal conflicts ----------------------------------------------------------------

# createDiagValues returns a vector identifying diagonal attack rows
# idea for speeding this up: cache diagonals
createDiagValues <- function(select_row, select_col, up_or_down = 1) {
  padWithZeros <- c(rep.int(0,times = select_col))
  
  diag_conflicts <- seq.int(from = select_row + up_or_down, 
                            by = up_or_down, 
                            length.out = 8 - select_col)
  diag_conflicts <- replace(diag_conflicts, diag_conflicts < 0, 0)
  diag_conflicts <- replace(diag_conflicts, diag_conflicts > 8, 0)
  
  return(c(padWithZeros,diag_conflicts))
}


is_this_a_valid_8_queens <- function(potentialSolution) {
  
  # check for diagonal conflicts
  for (eachElement in 1:8) {
    diagonal_conflicts <- createDiagValues(potentialSolution[eachElement], eachElement, 1)
    areThereDiagonalConflicts <- potentialSolution - diagonal_conflicts
    if (any(areThereDiagonalConflicts == 0)) {return(FALSE)}
    
    diagonal_conflicts <- createDiagValues(potentialSolution[eachElement], eachElement, -1)
    areThereDiagonalConflicts <- potentialSolution - diagonal_conflicts
    if (any(areThereDiagonalConflicts == 0)) {return(FALSE)}
  }
  
  # if we made it this far, it's a valid solution
  return(TRUE)
}



# run the complete series ----
# build a table containing all possible iterations
# placing 1-8 in each column prevents horizontal violations
# Permutation (instead of expand.grid) prevents row duplications, so don't need to check that violation
allPossibleSolutions <- permn(1:8)

validSolutions <- rep(NA, length(allPossibleSolutions)) # is a row a valid solution? T or F

runQueen <- function() {
  progressBar <- txtProgressBar()
  allPossSoln_length <- length(allPossibleSolutions)
  for(queenRow in 1:allPossSoln_length) {
    setTxtProgressBar(progressBar, queenRow/allPossSoln_length)
    validSolutions[queenRow] <<- is_this_a_valid_8_queens(allPossibleSolutions[[queenRow]])
  }
}

runQueen()

allValidSolutions <- allPossibleSolutions[validSolutions]

# Plot the solutions ------------------------------------------------------

progressBar <- txtProgressBar(style = 3)
for (queenPlot in 1:length(allValidSolutions)) {
  plotARowOfQueens(allValidSolutions[[queenPlot]])
  setTxtProgressBar(progressBar,queenPlot/length(allValidSolutions))
  Sys.sleep(1)
}


###Problem 4: Build a musical instrument
#accessing peripherals 

###Problem Statement 
#create a computer program that converts mouse movements
#into musical pitches. The instrument should be silent until one
#of the mouse buttons is held down and move it 
#up and down to change the pitch and move it side to side to change
#the volume. Let go of the mouse button and the musical tone stops.

###Solution
#if many real time applications always better to fo
#it in lower level languages such as c, c++ or assembler

# https://stat.ethz.ch/R-manual/R-devel/library/tcltk/html/tcltk-package.html

library(tcltk) #to capture the mouse movements 
library(sonify) #to convert to music 

#initialize tcl/tk
mouseCapture <- data.frame(xMouse = numeric(0), 
                           yMouse = numeric(0))
startMoveTime <- NULL

handleMouse <- function(x, y) {
  # <B1-Motion>
  
  if (is.null(startMoveTime)) {
    startMoveTime <<- Sys.time()
  }
  # cache the mouse location
  localMC <- mouseCapture
  newDF <- data.frame(xMouse = as.numeric(x), 
                      yMouse = as.numeric(y))
  mouseCapture <<- rbind(localMC, newDF)
}

# play sound 

playSound <- function() {
  #<ButtonRelease-1>
  moveTime <- as.numeric(Sys.time() - startMoveTime)
  
  # invert the y axis so pitch goes "up"
  mouseCapture <<- mouseCapture[nrow(mouseCapture):1,]
  
  # convert the data to sound
  sonify(mouseCapture$yMouse, 
         duration = moveTime,
         pulse_amp = mouseCapture$xMouse/255 )
  
}

# start the theremin ------------------------------------------------------


runTheremin <- function() {
  # reset the counters
  mouseCapture <<- data.frame(xMouse = numeric(0), 
                              yMouse = numeric(0))
  startMoveTime <<- NULL
  
  # set up tkinter
  tt <- tktoplevel()
  tkbind(tt,"<B1-Motion>", handleMouse )
  tkbind(tt,"<ButtonRelease-1>", playSound )
  tkfocus(tt)
  
}

runTheremin()


###Problem 5: Facial Recognition
#accessing peripherals 

###Problem Statement 
#the function should accept a JPEG, GIF or PNG file of 
#varying dimensions and then should identify the number of
#faces in that image and then create a copy of the image 
#wherein their surrounding box comes in 

###Solution
#aspect of machine learning 
#uses microsoft cognitive services to recognize faces in an image 
#first call the API then create the image with the boxes 

library(rjson)
library(httr)
library(imager)

# Web API -----------------------------------------------------------------


recognizeFaces <- function(imageWithFaces) {
  # imageWithFaces is a URL to a jpeg, gif or png
  
  endpoint <- "https://westcentralus.api.cognitive.microsoft.com/face/v1.0/detect"
  FaceRecogURL <- "?returnFaceId=true&returnFaceRectangle=true"
  
  theURLtoSend <- paste0(endpoint,FaceRecogURL)
  theBodytoSend <- paste0('{"url": "',imageWithFaces,'"}')
  
  # get a subscription key - https://docs.microsoft.com/en-us/azure/cognitive-services/computer-vision/vision-api-how-to-topics/howtosubscribe
  apiKey <- "put your subscription key here"
  
  thePostResults <- POST(theURLtoSend, 
                         add_headers( "Ocp-Apim-Subscription-Key" = apiKey ),
                         body = theBodytoSend,
                         encode = "json"
  )
  thePRfromJSON <- fromJSON(content(thePostResults, "text"))
  
  
  # display image with boxes
  storeImageHere <- file.path(getwd(),"tempfacefile")
  download.file(imageWithFaces, storeImageHere, mode='wb')
  hereIsImage <- load.image(storeImageHere)
  
  facesWithBoxes <- file.path(getwd(), "facesWithBoxes.png")
  
  # save plot to disk file
  png(filename = facesWithBoxes)
  plot(hereIsImage)
  for (eachface in 1:length(thePRfromJSON)) {
    theRect <- thePRfromJSON[[eachface]]$faceRectangle
    rect(xleft = theRect$left, 
         ybottom = theRect$top + theRect$height,
         xright = theRect$left + theRect$width,
         ytop = theRect$top
    )
  }
  dev.off()
  
  
  # return face count
  results <- list(countfaces = length(thePRfromJSON), 
                  imageLocation = facesWithBoxes)
  write(toJSON(results), file = "faceData.json")
}

recognizeFaces("https://www.nasa.gov/sites/default/files/iss038-s-002.jpg")
