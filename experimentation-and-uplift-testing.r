#### TASK 2 Experiment and Uplift Testing
# Load dataset with URL or Use Output from Task 1, it same
# download.file("https://cdn.theforage.com/vinternships/companyassets/32A6DqtsbF7LbKdcq/QVI_data.csv", "D:/Personal Improvement/Forage/1. Quantium/QVI_transaction.csv")

filePath <- "D:/Personal Improvement/Forage/1. Quantium/"
dataset <- fread(paste0(filePath, "QVI_transaction.csv"))
dataset

#### Set themes for plots
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

library(dplyr)
data[, YEARMONTH := year(DATE)*100 + month(DATE)]
# data <- subset(data, select = -c(YEARMINTH))
View(dataset)
View(data)
#### Measure calculations
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Filter to the pre-trial periode and stores with full observations periods
storeWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storeWithFullObs, ]

#### Create a function to calculate correlation for a measure
#### Looping through each control store
calculateCorrelation <- function(inputTable, metricCol, storeComparison){
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())

  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i, "corr_measure" = cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)], inputTable[STORE_NBR == i, eval(metricCol)]))
    
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
  }
  return(calcCorrTable)
}

#### Create a function to calculate a standardised magnitude distance for a measure
#### Looping through each control store
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    
    calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i, "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH], "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)] - inputTable[STORE_NBR == i, eval(metricCol)])
                                   )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }

#### Standardise the magnitude distance so that the measure ranges from 0 to 1
minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = c("Store1", "YEARMONTH")]
distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]

finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
return(finalDistTable)
}

#### Use the function for calculating correlation
trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

#### Use the functions for calculating magnitude
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

#### Create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))[, scoreNSales := corr_measure * corr_weight + mag_measure + (1-corr_weight)]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))[, scoreNCust := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]

#### Combine scores across the drivers
score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closet to 1 but not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 77
control_store <- score_Control[Store1 == trial_store, ][order(-finalControlScore)][2, Store2]
control_store

#### Visual checks on trends based on the drivers with total sales
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                  ][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
                                    ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                      ][YEARMONTH < 201903, ]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) + 
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#### Visual checks on trends based on the drivers with number of customers
measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                       ][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                         ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1 , sep = "-"), "%Y-%m-%d")
                                           ][YEARMONTH < 201903, ]

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")

#### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ , controlSales := totSales * scalingFactorForControlSales]

#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")], measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales)/controlSales]

#### As our null hypothesis is that the trial periode is the same as the pre-trial period, Let's take the standard deviation based on the scaled percentage difference in the pre-trial periode
stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

#### Nite that there are 8 months in the pre-trial period hence 8 - 1 = 7 degrees of freedom
degreesOfFreedom <- 7

#### We will test with a null hypothesis of there being 0 difference between trial and control stores
percentageDiff[, tValue := (percentageDiff - 0)/stdDev
               ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                 ][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

#### Find the 95th percentile of the t distribution with the appropriate degrees of freedom to compare against
qt(0.95, df = degreesOfFreedom)

measureOverTimeSales <- measureOverTime
#### Trial and control store total sales
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                  ][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
                                    ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                      ][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
                                 ][, totSales := totSales * (1 + stdDev * 2)
                                   ][, Store_type := "Control 95th % Cofidence interval"]

### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % Cofidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL),
            show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#### THis would be a repeat of the steps before for total sales
#### Scale pre-trial control customers to match pre-trial trial store customers
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

#### Apply the scaling factor
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store][, controlCustomers := nCustomers * scalingFactorForControlCust][, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))]

#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")], 
                        measureOverTimeCusts[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")],
                        by = "YEARMONTH"
                        )[, percentageDiff := abs(controlCustomers - nCustomers)/controlCustomers]

#### As our null hypothesis is that the trial periode is the same as the pre-trial period, let's take the standard deviation based on the scaled percentage difference in the pre-trial periode
stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

degreesOfFreedom <- 7

#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                      ][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales_Controls95 <- pastCustomers[Store_type == "Control",][, nCusts := nCusts * (1 + stdDev * 2)
                                                                ][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastSales_Controls5 <- pastCustomers[Store_type == "Control",][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastSales_Controls95, pastSales_Controls5)
trialAssessment

#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")


#### TRIAL STORE 86
#### Measure calculation
measureOverTime86 <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Use the function for calculating correlation
trial_store88 <- 86
corr_nSales86 <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store86)
corr_nCustomers86 <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store86)

#### Use the functions for calculating magnitude
magnitude_nSales86 <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store86)
magnitude_nCustomers86 <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store86)

#### Create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales86 <- merge(corr_nSales86, magnitude_nSales86, by = c("Store1", "Store2"))[, scoreNSales := corr_measure * corr_weight + mag_measure + (1-corr_weight)]
score_nCustomers86 <- merge(corr_nCustomers86, magnitude_nCustomers86, by = c("Store1", "Store2"))[, scoreNCust := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]

#### Combine scores across the drivers
score_Control86 <- merge(score_nSales86, score_nCustomers86, by = c("Store1", "Store2"))
score_Control86[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closet to 1 but not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 86
control_store86 <- score_Control86[Store1 == trial_store86, ][order(-finalControlScore)][2, Store2]
control_store86

#### Visual checks on trends based on the drivers with total sales
measureOverTimeSales86 <- measureOverTime86
pastSales86 <- measureOverTimeSales86[, Store_type := ifelse(STORE_NBR == trial_store86, "Trial", ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastSales86, aes(TransactionMonth, totSales, color = Store_type)) + 
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#### Visual checks on trends based on the drivers with number of customers
measureOverTimeCusts86 <- measureOverTime86
pastCustomers86 <- measureOverTimeCusts86[, Store_type := ifelse(STORE_NBR == trial_store86, "Trial", ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1 , sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastCustomers86, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")

#### Scale pre-trial control sales to match pre-trial trial store 86 sales
scalingFactorForControlSales86 <- preTrialMeasures[STORE_NBR == trial_store86 & YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store86 & YEARMONTH < 201902, sum(totSales)]

#### Apply the scaling factor
measureOverTimeSales86 <- measureOverTime86
scaledControlSales86 <- measureOverTimeSales86[STORE_NBR == control_store86, ][ , controlSales := totSales * scalingFactorForControlSales86]

#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff86 <- merge(scaledControlSales86[, c("YEARMONTH", "controlSales")], measureOverTime86[STORE_NBR == trial_store86, c("totSales", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff86 := abs(controlSales - totSales)/controlSales]

#### As our null hypothesis is that the trial periode is the same as the pre-trial period, Let's take the standard deviation based on the scaled percentage difference in the pre-trial periode
stdDev86 <- sd(percentageDiff86[YEARMONTH < 201902, percentageDiff86])

#### Nite that there are 8 months in the pre-trial period hence 8 - 1 = 7 degrees of freedom
degreesOfFreedom <- 7


#### We will test with a null hypothesis of there being 0 difference between trial and control stores
percentageDiff86[, tValue := (percentageDiff86 - 0)/stdDev86
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

#### Find the 95th percentile of the t distribution with the appropriate degrees of freedom to compare against
qt(0.95, df = degreesOfFreedom)

#### Trial and control store total sales
measureOverTimeSales86 <- measureOverTime86
pastSales86 <- measureOverTimeSales86[, Store_type := ifelse(STORE_NBR == trial_store86, "Trial", ifelse(STORE_NBR == control_store86, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales86_Controls95 <- pastSales86[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % Cofidence interval"]

### Control store 5th percentile
pastSales86_Controls5 <- pastSales86[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % Cofidence interval"]

trialAssessment86 <- rbind(pastSales86, pastSales86_Controls95, pastSales86_Controls5)

#### Plotting these in one nice graph
ggplot(trialAssessment86, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment86[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL),
            show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#### THis would be a repeat of the steps before for total sales
#### Scale pre-trial control customers to match pre-trial trial store customers
scalingFactorForControlCust86 <- preTrialMeasures[STORE_NBR == trial_store86 & YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store86 & YEARMONTH < 201902, sum(nCustomers)]

#### Apply the scaling factor
measureOverTimeCusts86 <- measureOverTime86
scaledControlCustomers86 <- measureOverTimeCusts86[STORE_NBR == control_store86][, controlCustomers := nCustomers * scalingFactorForControlCust86][, Store_type := ifelse(STORE_NBR == trial_store86, "Trial", ifelse(STORE_NBR == control_store86, "Control", "Other stores"))]

#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff86 <- merge(scaledControlCustomers86[, c("YEARMONTH", "controlCustomers")], 
                        measureOverTimeCusts86[STORE_NBR == trial_store86, c("nCustomers", "YEARMONTH")],
                        by = "YEARMONTH"
)[, percentageDiff86 := abs(controlCustomers - nCustomers)/controlCustomers]

#### As our null hypothesis is that the trial periode is the same as the pre-trial period, let's take the standard deviation based on the scaled percentage difference in the pre-trial periode
stdDev86 <- sd(percentageDiff86[YEARMONTH < 201902, percentageDiff86])

degreesOfFreedom <- 7

#### Trial and control store number of customers
pastCustomers86 <- measureOverTimeCusts86[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales86_Controls95 <- pastCustomers86[Store_type == "Control",][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastSales86_Controls5 <- pastCustomers86[Store_type == "Control",][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment86 <- rbind(pastCustomers86, pastSales86_Controls95, pastSales86_Controls5)
trialAssessment86

#### Plotting these in one nice graph
ggplot(trialAssessment86, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment86[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")


#### TRIAL STORE 88
#### Measure calculation
measureOverTime88 <- data[, .(totSales = sum(TOT_SALES),
                              nCustomers = uniqueN(LYLTY_CARD_NBR),
                              nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                              nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                              avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Use the function for calculating correlation
trial_store88 <- 88
corr_nSales88 <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store88)
corr_nCustomers88 <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store88)

#### Use the functions for calculating magnitude
magnitude_nSales88 <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store88)
magnitude_nCustomers88 <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store88)

#### Create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales88 <- merge(corr_nSales88, magnitude_nSales88, by = c("Store1", "Store2"))[, scoreNSales := corr_measure * corr_weight + mag_measure + (1-corr_weight)]
score_nCustomers88 <- merge(corr_nCustomers88, magnitude_nCustomers88, by = c("Store1", "Store2"))[, scoreNCust := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]

#### Combine scores across the drivers
score_Control88 <- merge(score_nSales88, score_nCustomers88, by = c("Store1", "Store2"))
score_Control88[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closet to 1 but not the store itself, i.e. the second ranked highest store)
#### Select control store for trial store 88
control_store88 <- score_Control88[Store1 == trial_store88, ][order(-finalControlScore)][2, Store2]
control_store88

#### Visual checks on trends based on the drivers with total sales
measureOverTimeSales88 <- measureOverTime88
pastSales88 <- measureOverTimeSales88[, Store_type := ifelse(STORE_NBR == trial_store88, "Trial", ifelse(STORE_NBR == control_store88, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastSales88, aes(TransactionMonth, totSales, color = Store_type)) + 
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


#### Visual checks on trends based on the drivers with number of customers
measureOverTimeCusts88 <- measureOverTime88
pastCustomers88 <- measureOverTimeCusts88[, Store_type := ifelse(STORE_NBR == trial_store88, "Trial", ifelse(STORE_NBR == control_store88, "Control", "Other stores"))
][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1 , sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastCustomers88, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")

#### Scale pre-trial control sales to match pre-trial trial store 88 sales
scalingFactorForControlSales88 <- preTrialMeasures[STORE_NBR == trial_store88 & YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store88 & YEARMONTH < 201902, sum(totSales)]

#### Apply the scaling factor
measureOverTimeSales88 <- measureOverTime88
scaledControlSales88 <- measureOverTimeSales88[STORE_NBR == control_store88, ][ , controlSales := totSales * scalingFactorForControlSales88]

#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff88 <- merge(scaledControlSales88[, c("YEARMONTH", "controlSales")], measureOverTime88[STORE_NBR == trial_store88, c("totSales", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff88 := abs(controlSales - totSales)/controlSales]

#### As our null hypothesis is that the trial periode is the same as the pre-trial period, Let's take the standard deviation based on the scaled percentage difference in the pre-trial periode
stdDev88 <- sd(percentageDiff88[YEARMONTH < 201902, percentageDiff88])

#### Nite that there are 8 months in the pre-trial period hence 8 - 1 = 7 degrees of freedom
degreesOfFreedom <- 7


#### We will test with a null hypothesis of there being 0 difference between trial and control stores
percentageDiff88[, tValue := (percentageDiff88 - 0)/stdDev88
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

#### Find the 95th percentile of the t distribution with the appropriate degrees of freedom to compare against
qt(0.95, df = degreesOfFreedom)

#### Trial and control store total sales
measureOverTimeSales88 <- measureOverTime88
pastSales88 <- measureOverTimeSales88[, Store_type := ifelse(STORE_NBR == trial_store88, "Trial", ifelse(STORE_NBR == control_store88, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales88_Controls95 <- pastSales88[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % Cofidence interval"]

### Control store 5th percentile
pastSales88_Controls5 <- pastSales88[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % Cofidence interval"]

trialAssessment88 <- rbind(pastSales88, pastSales88_Controls95, pastSales88_Controls5)

#### Plotting these in one nice graph
ggplot(trialAssessment88, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment88[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL),
            show.legend = FALSE) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#### THis would be a repeat of the steps before for total sales
#### Scale pre-trial control customers to match pre-trial trial store customers
scalingFactorForControlCust88 <- preTrialMeasures[STORE_NBR == trial_store88 & YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR == control_store88 & YEARMONTH < 201902, sum(nCustomers)]

#### Apply the scaling factor
measureOverTimeCusts88 <- measureOverTime88
scaledControlCustomers88 <- measureOverTimeCusts88[STORE_NBR == control_store88][, controlCustomers := nCustomers * scalingFactorForControlCust88][, Store_type := ifelse(STORE_NBR == trial_store88, "Trial", ifelse(STORE_NBR == control_store88, "Control", "Other stores"))]

#### Calculate the percentage difference between scaled control sales and trial sales
percentageDiff88 <- merge(scaledControlCustomers88[, c("YEARMONTH", "controlCustomers")], 
                          measureOverTimeCusts88[STORE_NBR == trial_store88, c("nCustomers", "YEARMONTH")],
                          by = "YEARMONTH"
)[, percentageDiff88 := abs(controlCustomers - nCustomers)/controlCustomers]

#### As our null hypothesis is that the trial periode is the same as the pre-trial period, let's take the standard deviation based on the scaled percentage difference in the pre-trial periode
stdDev88 <- sd(percentageDiff88[YEARMONTH < 201902, percentageDiff88])

degreesOfFreedom <- 7

#### Trial and control store number of customers
pastCustomers88 <- measureOverTimeCusts88[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales88_Controls95 <- pastCustomers88[Store_type == "Control",][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastSales88_Controls5 <- pastCustomers88[Store_type == "Control",][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment88 <- rbind(pastCustomers88, pastSales88_Controls95, pastSales88_Controls5)
trialAssessment88

#### Plotting these in one nice graph
ggplot(trialAssessment88, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment88[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total number of customers by month")
