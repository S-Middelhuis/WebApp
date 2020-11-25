# This file contains functions used for the
# cox proportional hazard model and the functions
# necessary for the web application. 

#' @title runScript
#' @description Run main script 
#' @param deps Selected departments
#' @export
startScript <- function(deps){
ptm <- proc.time()

# Folder where data is stored
networkPath <- "\\\\nas001\\MMC-ALG\\Data-analyse-BI\\Building Blocks\\Planners\\Data"

# Libraries
library(data.table)
library(plyr)
library(dplyr)
library(readstata13)
library(ggplot2)
library(hydroGOF)
library(survival)
library(readxl)
library(lubridate)
source("functions.r")

deps = as.integer(deps)
deps[deps == 1] <- "2A"
deps[deps == 2] <- "2B"
deps[deps == 3] <- "2C"
deps[deps == 4] <- "2D"

# Load data
train <- loadTrainData()
data  <- loadData(networkPath)

# Make overview 
getOverview(data$iclist, data$dataPlanning, deps)

# Pre-format data 
train <- train[train$afdelingcode %in% c("2A", "2B", "2C", "2D"), ]
data$dataPlanning <- data$dataPlanning %>%
  filter(reserveringsafdeling %in% deps | grepl("Carotis", okverrichtingomschrijving) == T) %>%
  filter(opnemendspecialisme != "GYN") %>%
  mutate(opnamedatum = as.Date(opnamedatum, format = "%d-%m-%Y"))
data$dataCurrent <- data$dataCurrent %>%
  filter(afdelingcode %in% deps) %>%
  filter(okprioriteitcodeomschrijving == "Electief (planbaar)")
recentPlanning <- data$dataPlanning %>%
  filter(data$dataPlan$laatstemutatiedatumtijd == max(data$dataPlan$laatstemutatiedatumtijd))

# Data corrections and column selection
dataTrain <- formatTrain(train)
dataCurr  <- formatTest(data$dataCurrent, dataTrain)
dataPlan  <- formatTest(data$dataPlan, dataTrain)
dataWacht <- formatTest(data$wachtlijst, dataTrain)

write.csv(dataTrain, '../Data/exports/dataTrain.csv')
write.csv(dataWacht, '../Data/exports/wachtlijst.csv')
write.csv(dataCurr, '../Data/exports/dataCurrent.csv')
write.csv(data$dataPlanning, '../Data/exports/dataplanning.csv')

# Load cox proportional hazard model
load("../Data/fitCox.RData")

# Get predictions for current and planned patients
# Current patients
predictions.current <- survfit(fitCox, newdata = dataCurr)
dailyPredictions.current <- predictions.current$surv
colnames(dailyPredictions.current) <- dataCurr$opnamenummer
# Planned patients
predictions.planning <- survfit(fitCox, newdata = dataPlan)
dailyPredictions.planning <- predictions.planning$surv
colnames(dailyPredictions.planning) <- dataPlan$opnamenummer

# Fill planning with current patients and planned patients
results.planning <- formatOutputPlanning(dailyPredictions.planning, dailyPredictions.current, dataPlan, dataCurr)
results.recent   <- results.planning[, -which(names(results.planning) %in% as.character(recentPlanning$opnamenummer))]
results.current  <- results.planning[, which(names(results.planning[,-1]) %in% as.character(dataCurr$opnamenummer))]

# Write results to data frame
fc.current  <- cbind(data.frame(results.current$tDate), data.frame(rowSums(results.current[,-1])))
fc.planning <- cbind(data.frame(results.planning$tDate),data.frame(rowSums(results.planning[,-1])))
fc.recent   <- cbind(data.frame(results.recent$tDate), data.frame(rowSums(results.recent[,-1])))

# Keep data upward of today
fc.current  <- fc.current[fc.current[,1] >= Sys.Date(), ]
fc.planning <- fc.planning[fc.planning[,1] >= Sys.Date(), ]
fc.recent   <- fc.recent[fc.recent[,1] >= Sys.Date(), ]

# Bind data and rename
fc <- cbind(fc.current,fc.planning[,2], fc.recent[,2])
colnames(fc)  <- c("dates","prob.c", "prob.p", "prob.r")

# Add red line to show maximum capacity
maxWeekend <- 8
maxWeekday <- 14
fc$max <- ifelse(weekdays(fc$dates) %in% c("zaterdag", "zondag"), maxWeekend, maxWeekday)

# Save current and initial bed occupation to csv files
write.csv(fc, '../Data/exports/fc.csv')

# Prepare data for app tabs
getCurrent(data$dataCurrent, deps)
print(proc.time() - ptm)
return(fc)
}


#' @title Load training data.
#' @description Function to load the data from directory.
#' @param path
#' @export
loadTrainData <- function(){
  # Read all train files
  dat16 <- read.dta13('../Data/training sets/2016.dta')
  dat17 <- read.dta13('../Data/training sets/2017.dta')
  dat18 <- read.dta13('../Data/training sets/2018.dta')
  
  # Fix that columns are the same for each year
  dat16 <- dat16[,colnames(dat16) %in% colnames(dat17)]
  dat16 <- dat16[,colnames(dat16) %in% colnames(dat18)]

  dat17 <- dat17[,colnames(dat17) %in% colnames(dat16)]
  dat17 <- dat17[,colnames(dat17) %in% colnames(dat18)]
  
  dat18 <- dat18[,colnames(dat18) %in% colnames(dat16)]
  dat18 <- dat18[,colnames(dat18) %in% colnames(dat17)]
  
  # Bind years together
  datTrain <- rbind(dat16, dat17, dat18)
  
  return(datTrain)
}


#' @title Load data.
#' @description Function to load the data from network path.
#' @param path
#' @export
loadData <- function(path){
  
  # Read all files from network location
  datCurrent  <- read_excel(file.path(path, "Huidig_Bedbezetting_Beddenhuis.xlsx"))
  datPlanning <- read_excel(file.path(path, "Toekomstige_Bedbezetting_Beddenhuis.xlsx"))
  wachtlijst  <- read_excel(file.path(path, "Wachtlijst_tbv_beddenhuis.xlsx"))
  iclist      <- read_excel(file.path(path, "Postoperatief_bestemming_IC_MC.xlsx"))
  
  # Fix date time in planning data
  datPlanning$laatstemutatiedatumtijd <- as.POSIXct(datPlanning$laatstemutatiedatumtijd, format = "%d-%m-%Y %H:%M")
  datPlanning$laatstemutatiedatumtijd <- as.Date(datPlanning$laatstemutatiedatumtijd)
  
  return(list("dataCurrent" = datCurrent, "dataPlanning" = datPlanning, "wachtlijst" = wachtlijst, "iclist" = iclist))
}


#' @title Formatting training data.
#' @description Function to select the right variables, and edit the variables when necessary.
#' @param data The dataframe.
#' @export
formatTrain <- function(data){
  data <- data %>%
    # Keep only tag_row equal to 1
    filter(tag_row == 1) %>%
    # Variables to select
    select(
      opnamenummer,
      operatiedatum,
      length_of_stay_afd,
      patientleeftijd,
      patientgeslacht,
      diabetes,
      roken,
      opnemendspecialisme,
      behandelendartsnaam,
      anesthesietechniek
    ) %>%
    
    # Make date of operatiedatum variable
    mutate(operatiedatum = as.Date(operatiedatum)) %>%
    mutate(length_of_stay_afd = ceiling(length_of_stay_afd)) %>%
    # For all behandelendartsnaam appearing less than 26 times, set equal to category "other"
    group_by(behandelendartsnaam) %>%
    mutate(count = n()) %>%
    ungroup %>%
    mutate(behandelendartsnaam = ifelse(count > 25, behandelendartsnaam, "other")) %>%
    select(-count) %>%
    # For all opnemendspecialisme appearing less than 26 times, set equal to category "other"
    group_by(opnemendspecialisme) %>%
    mutate(count = n()) %>%
    ungroup %>%
    mutate(opnemendspecialisme = ifelse(count > 25, opnemendspecialisme, "other")) %>%
    select(-count) %>%
    # For all anesthesietechniek appearing less than 26 times, set equal to category "other"
    group_by(anesthesietechniek) %>%
    mutate(count = n()) %>%
    ungroup %>%
    mutate(anesthesietechniek = ifelse(count > 25, anesthesietechniek, "other")) %>%
    select(-count) %>% 
    # Remove observations without operatiedatum
    filter(!is.na(operatiedatum)) %>%
    # Remove duplicate id rows
    distinct(opnamenummer, .keep_all = T)
  
  colnames(data)[which(names(data) == "length_of_stay_afd")] <- "length_of_stay" 
  return(data)
}


#' @title Formatting test data.
#' @description Function to select the right variables, and edit the variables when necessary.
#' @param data The dataframe.
#' @param hist Training dataframe
#' @export
formatTest <- function(data, hist){
   data <- data %>% 
     # Variables to select 
     select(
       opnamenummer,
       opnamedatum,
       patientleeftijd,
       patientgeslacht,
       diabetes,
       roken,
       opnemendspecialisme,
       behandelendartsnaam,
       anesthesietechniek
     ) %>%
     # Remove duplicate id numbers
     distinct(opnamenummer, .keep_all = T)%>%
     mutate(patientleeftijd = as.numeric(patientleeftijd)) %>%
     # Change opnamedatum to date variable
     mutate(opnamedatum = as.Date(opnamedatum, format = "%d-%m-%Y")) %>%
     # Change diabetes answer to yes or no value
     mutate(diabetes = ifelse(diabetes %in% c("ja", "dieet/insuline", "alleen dieet"), "ja", "nee")) %>%
     # For all behandelendartsnaam not appearing in training data, set equal to category "other"
     mutate(behandelendartsnaam = ifelse(behandelendartsnaam %in% hist$behandelendartsnaam, behandelendartsnaam, "other")) %>%
     # For all opnemendspecialisme not appearing in training data, set equal to category "other"
     mutate(opnemendspecialisme = ifelse(opnemendspecialisme %in% hist$opnemendspecialisme, opnemendspecialisme, "other")) %>%
     # For all anesthesietechniek not appearing in training data, set equal to category "other"
     mutate(anesthesietechniek = ifelse(anesthesietechniek %in% hist$anesthesietechniek, anesthesietechniek, "other")) %>%
     # For all roken answers not appearing in training data, set equal to category "other"
     mutate(roken = ifelse(roken %in% c("gestopt", "ja", "nooit", "nee"), roken, "nee"))
        
     data$diabetes[is.na(data$diabetes)] <- "nee"
     data$roken[is.na(data$roken)] <- "nooit"
     
     #mutate(diabetes = ifelse(diabetes == "NULL", "nee", diabetes)) %>%
     #mutate(roken = ifelse(roken == "NULL", "nooit", roken))
   return(data)
}


#' @title Initializing output.
#' @description Create an output vector which starts at the minimum date in the dataframe,
#' and ends at a specific number of days after the system date, with daily increments.
#' If you want to change the validation to be more frequent, the maximum date should be adjusted.
#' @param data Dateframe.
#' @export
initializeOutput <- function(data){
  dateMin <- min(data$opnamedatum, na.rm = T)
  dateMax <- Sys.Date()+35
  out <- data.frame(tDate = seq(dateMin, dateMax, by = "day"))
}


#' @title Results of the current occupation dataset
#' @description Creates the current occupation results by creating one big dataframe in which the actual
#' length of stay and the predicted length of stay are given. 
#' @param dailyPredictions Predictions resulting from the cox proportional hazard model. 
#' @param data Dataframe.
#' @export
formatOutputCurrent <- function(dailyPredictions, data){
  # Initialize result dataframes
  tmp_prob <- initializeOutput(data)
  for (i in 1:ncol(dailyPredictions)){
    # Get start date, true length, and id
    startDate <- data$opnamedatum[i]
    id <- data$opnamenummer[i]
    # Create date sequence for individual predictions
    individualPredictions <- data.frame(tDate = seq(startDate, startDate + nrow(dailyPredictions), by = "day")) #64
    individualPredictions$new <- c(1, dailyPredictions[, i])
    colnames(individualPredictions)[2] <- id
    # Join results to set
    tmp_prob <- tmp_prob %>% left_join(individualPredictions)
  }
  # Replace all na values with 0
  tmp_prob[is.na(tmp_prob)] <- 0
  return(tmp_prob)
}


#' @title Get planning
#' @description Function to obtain the results of the planning
#' @param dailPredictions.planning Cox results of planned patients
#' @param dailyPredictions.occ Cox results of current patients
#' @param data The dataframe
#' @param current The current occupation
#' @export
formatOutputPlanning <- function(dailyPredictions.planning, dailyPredictions.occ, data, current){
  # Initialize result dataframe
  tmp <- initializeOutput(current)
  # bind the two cox results 
  dailyPredictions <- cbind(dailyPredictions.occ, dailyPredictions.planning)
  dataTotal <- rbind(current, data)
  
  for (i in 1:ncol(dailyPredictions)){
    # Get start date, true length, and id
    startDate <- dataTotal$opnamedatum[i]
    id <- dataTotal$opnamenummer[i]
    # Create date sequence for individual predictions
    individualPredictions <- data.frame(tDate = seq(startDate, startDate + nrow(dailyPredictions), by = "day")) #64
    individualPredictions$new <- c(1, dailyPredictions[, i])
    colnames(individualPredictions)[2] <- id
    # Join results to set
    tmp <- tmp %>% left_join(individualPredictions)
  }
  # Replace all na values with 0
  tmp[is.na(tmp)] <- 0
  return(tmp)
}


#' @title Get day planning
#' @description Provides number of incoming patients on a certain day
#' @param date the date
#' @export
getDayPlanning <- function(date){
  data.plan <- read.csv('../Data/exports/dataplanning.csv')
  data.plan$opnamedatum <- as.Date(data.plan$opnamedatum)
  date <- as.Date(as.numeric(date))
  dataPlan <- data.plan %>%
    filter(opnamedatum == date)
  if(nrow(dataPlan) != 0){
    dataPlan <- dataPlan %>%
      mutate(X = seq(1, nrow(dataPlan)))
  }
  
  dataPlan <- dataPlan %>%
    select(
      X,
      opnamenummer,
      opnamedatum,
      patientleeftijd,
      patientgeslacht,
      diabetes,
      roken,
      reserveringsafdeling,
      okverrichtingomschrijving,
      postoperatievebestemming,
      opnemendspecialisme,
      behandelendartsnaam,
      anesthesietechniek,
      laatstemutatiedatumtijd,
      operatienr,
      geplandeoperatiedatum,
      BEHINFO,
      statusomschrijving)
  
  dataPlan$opnamedatum <- as.character(dataPlan$opnamedatum)
  return(dataPlan)
}


#' @title Get current
#' @description Function to obtain the table with all current patients
#' @param data
#' @param deps
#' @export
getCurrent <- function(data, deps){
  current <- data %>%
    filter(statusomschrijving == "Opgenomen") %>%
    filter(afdelingcode %in% deps) %>%
    distinct(opnamenummer, .keep_all = T)
  write.csv(current, '../Data/exports/current.csv')
}


#' @title Get overview
#' @description Function to obtain the table with an overview of the different specialisms per week
#' @param data IC dataset
#' @param dataPlan data planning 
#' @param deps Selected departments
#' @export
getOverview <- function(dataIC, dataPlan, deps){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  wk <- floor_date(Sys.Date(), "week")+1
  
  datIC <- dataIC %>%
    mutate( geplandeopnamedatum = as.Date(geplandeopnamedatum, format="%Y-%m-%d %H:%M:%OS") ) %>%
    mutate( icdate = ifelse(VER_CODE == "33450" & weekdays(geplandeopnamedatum) != "vrijdag", geplandeopnamedatum+1,  geplandeopnamedatum) ) %>%
    mutate( icdate = as.Date(icdate) ) %>%
    mutate( postoperatieve_bestemming = ifelse(postoperatieve_bestemming == "MC", "IC", postoperatieve_bestemming)) %>%
    filter( icdate >= wk & icdate <=  wk + 13) 
    
  
  dataPlan$opnamedatum <- as.Date(dataPlan$opnamedatum, format = "%d-%m-%Y")
  if (weekdays(Sys.Date()) == "vrijdag"){
    datSPEC <- dataPlan %>%
      filter( opnamedatum >= wk & opnamedatum <= wk + 13 ) %>%
      filter( reserveringsafdeling %in% deps )
  } else {
    datSPEC <- dataPlan %>%
      filter( opnamedatum >= wk & opnamedatum <= wk + 6 ) %>%
      filter( reserveringsafdeling %in% deps )
  }
  
  # Generate and write tables
  spec <- table(data.frame(datSPEC$opnamedatum, datSPEC$opnemendspecialisme))
  ic   <- table(data.frame(datIC$icdate, datIC$postoperatieve_bestemming))
  write.csv(spec, '../Data/exports/overzichtSPEC.csv')
  write.csv(ic, '../Data/exports/overzichtIC.csv')
  
  # Merge ic table with date range for 2 weeks
  daterange <- as.Date(seq(Sys.Date(), wk+13, by = 1))
  total     <- data.frame("Datum" = daterange, "IC" = as.integer(numeric(length(daterange))))
  
  ic = read.csv('../Data/exports/overzichtIC.csv', header = T)
  data.frame(ic)
  colnames(ic) = c("Datum", "IC")
  ic$Datum <- as.Date(ic$Datum)
  
  # Match ic table
  setDT(total)
  setDT(ic)
  total[ic, on = c("Datum"), IC := i.IC]
  total <- total %>%
    filter((weekdays(Datum) %!in% c('zaterdag','zondag')))
  write.csv(total, '../Data/exports/overzichtIC.csv')
  
}


#' @title Reload
#' @description Function to reload the dataset
#' @param deps selected departments
#' @export
reload <- function(deps){
  ptm <- proc.time()
  networkPath <- "\\\\nas001\\MMC-ALG\\Data-analyse-BI\\Building Blocks\\Planners\\Data"
  
  # Load new data
  data <- loadData(networkPath)
  
  # Change format selected departments
  deps = as.integer(deps)
  deps[deps == 1] <- "2A"
  deps[deps == 2] <- "2B"
  deps[deps == 3] <- "2C"
  deps[deps == 4] <- "2D"
  
  # Make overview
  getOverview(data$iclist, data$dataPlanning, deps)
  
  # Check if dataset is filled
  while( empty(data$dataPlanning) | empty(data$dataCurrent) ) {
    print("Empty dataframe")
    Sys.sleep(5)
    data <- loadData(networkPath)
  }
  print('Dataframe filled')  
  dataTrain <- read.csv('../Data/exports/dataTrain.csv')
  
  # Pre-format data 
  data$dataPlanning <- data$dataPlanning %>%
    filter(reserveringsafdeling %in% deps | grepl("Carotis", okverrichtingomschrijving) == T) %>%
    filter(opnemendspecialisme != "GYN") %>%
    mutate(opnamedatum = as.Date(opnamedatum, format = "%d-%m-%Y"))
  data$dataCurrent <- data$dataCurrent %>%
    filter(afdelingcode %in% deps) %>%
    filter(okprioriteitcodeomschrijving == "Electief (planbaar)")
  recentPlanning <- data$dataPlanning %>%
    filter(data$dataPlan$laatstemutatiedatumtijd == max(data$dataPlan$laatstemutatiedatumtijd))
  
  # Data corrections and column selection
  dataCurr  <- formatTest(data$dataCurrent, dataTrain)
  dataPlan  <- formatTest(data$dataPlan, dataTrain)
  dataWacht <- formatTest(data$wachtlijst, dataTrain)
  
  write.csv(dataWacht, '../Data/exports/wachtlijst.csv')
  write.csv(dataCurr, '../Data/exports/dataCurrent.csv')
  write.csv(data$dataPlan, '../Data/exports/dataplanning.csv')
  
  # Load cox proportional hazard model
  load("../Data/fitCox.RData")
  
  # Get predictions for current and planned patients
  # Current patients
  predictions.current <- survfit(fitCox, newdata = dataCurr)
  dailyPredictions.current <- predictions.current$surv
  colnames(dailyPredictions.current) <- dataCurr$opnamenummer
  # Planned patients
  predictions.planning <- survfit(fitCox, newdata = dataPlan)
  dailyPredictions.planning <- predictions.planning$surv
  colnames(dailyPredictions.planning) <- dataPlan$opnamenummer
  
  # Fill planning with current patients and planned patients
  results.planning <- formatOutputPlanning(dailyPredictions.planning, dailyPredictions.current, dataPlan, dataCurr)
  results.recent   <- results.planning[, -which(names(results.planning) %in% as.character(recentPlanning$opnamenummer))]
  results.current  <- results.planning[, which(names(results.planning[,-1]) %in% as.character(dataCurr$opnamenummer))]
  
  # Write results to data frame
  fc.current  <- cbind(data.frame(results.current$tDate), data.frame(rowSums(results.current[,-1])))
  fc.planning <- cbind(data.frame(results.planning$tDate),data.frame(rowSums(results.planning[,-1])))
  fc.recent   <- cbind(data.frame(results.recent$tDate), data.frame(rowSums(results.recent[,-1])))
  
  # Keep data upward of today
  fc.current  <- fc.current[fc.current[,1] >= Sys.Date(), ]
  fc.planning <- fc.planning[fc.planning[,1] >= Sys.Date(), ]
  fc.recent   <- fc.recent[fc.recent[,1] >= Sys.Date(), ]
  
  # Bind data and rename
  fc <- cbind(fc.current,fc.planning[,2], fc.recent[,2])
  colnames(fc)  <- c("dates","prob.c", "prob.p", "prob.r")
  
  # Add red line to show maximum capacity
  maxWeekend <- 8
  maxWeekday <- 14
  fc$max <- ifelse(weekdays(fc$dates) %in% c("zaterdag", "zondag"), maxWeekend, maxWeekday)
  write.csv(fc, '../Data/exports/fc.csv')

  # Prepare data for app tabs
  getCurrent(data$dataCurrent, deps)
  print(proc.time() - ptm)
  
  return(fc)
}


#' @title Plot results
#' @description Function to plot the results in the web application
#' @param data The data from the application
#' @export
plot_results <- function(data){
  # legend and color data
  legend  <- factor(c("Planning", "Meest recente mutaties"))
  palette <- c(rgb(120/255, 120/255, 120/255), rgb(20/255, 150/255, 40/255), rgb(60/255, 230/255, 90/255))
  
  #plotting
  diagram <- ggplot(data = data, aes(x=dates)) +    
    geom_bar(stat="identity", aes(y=prob.p, fill = "Recente mutaties")) +
    geom_bar(stat="identity", aes(y=prob.r, fill = "Planning")) +
    geom_bar(stat="identity", aes(y=prob.c, fill = "Huidige bezetting")) +
    scale_fill_manual("legenda", values = palette) + 
    #geom_line(aes(x=data$dates, y=data$max, color = "Max capaciteit")) +  
    #scale_color_manual("Legenda", values = "red") +
    xlab("Datum") +
    ylab("Bedden bezetting") +
    theme(legend.key = element_blank(),
          legend.title = element_blank())
          
  return(diagram)
}

#dataTrain$length_of_stay <- Surv(dataTrain$length_of_stay, rep(1, nrow(dataTrain)))
#summary(fitCox <- coxph(length_of_stay ~., dataTrain[,c(-1,-2)]))
#save(fitCox, file = "fitCox.RData"


