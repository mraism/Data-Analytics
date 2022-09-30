# Title : "Quantium Virtual Internship - Retail Strategy and Analytics - Task 1"

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(linewidth=80)
library(knitr)
hook_output = knit_hooks$get("output")
knit_hooks$set(output = function(x, options)
{
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth))
  {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n))
      x = strwrap(x, width = n)
    x = paste(x, collapse = "\n")
  }
  hook_output(x, options)
})

#### Load required libraries
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("ggmosaic")
#install.packages("readr")

library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)
library(curl)

#transactionData2 <- download.file("https://cdn.theforage.com/vinternships/companyassets/32A6DqtsbF7LbKdcq/QVI_transaction_data.xlsx", "D:/Personal Improvement/Forage/1. Quantium/QVI_transaction_data2.xlsx")

#### Point the filePath to where you have downloaded the datasets to and assign the data files to data.tables
filePath <- "D:/Personal Improvement/Forage/1. Quantium/"
transactionData <- fread(paste0(filePath, "QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath, "QVI_purchase_behaviour.csv"))

#### Examine Transaction Data
str(transactionData)
head(transactionData)
summary(transactionData)

#### Convert DATE column to a date format
#### A quick Google search tells us that CSV and Excel integer dates begin on 30 Dec 1899
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

head(customerData)
summary(customerData)

#### Examine PROD_NAME
transactionData[, .N, PROD_NAME]

#### Examine the words in PROD_NAME to see if there are any incorrect entries such as products that are not chips

productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')

#### Removing digits
#install.packages("ggrepel")
library("ggrepel")
productWords <- productWords[grepl("\\d", words) == FALSE, ]

#### Removing special characters
productWords <- productWords[grepl("[:alpha]", words), ]

#### Let's look at the most common words by counting the number of times a word appears and 
#### sorting them by this frequency in order of highest to lowest frequency
productWords[, .N, words][order(N, decreasing = TRUE)]

#### Remove SALSA products
#transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
#transactionData <- transactionData[SALSA == FALSE][, SALSA := NULL]

#### Summarise the data to check for nulls and possible outliers
summary(transactionData)

#### Filter the dataset to find the outlier
transactionData[PROD_QTY == 200, ]

#### Let's see if the customer has had other transactions
transactionData[LYLTY_CARD_NBR == 226000, ]

#### Filter out the customer based on the loyalty card number
transactionData <- transactionData[LYLTY_CARD_NBR != 226000, ]
#### Re-examince transaction data
summary(transactionData)

#### Count the number of transaction by date
transactionData[, .N, by = DATE]

#### Create a sequence of dates and join this the count of transactions by date
allDates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"), by = "day"))
setnames(allDates, "DATE")
transaction_by_day <- merge(allDates, transactionData[, .N, by = DATE], all.x = TRUE)

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Plot transactions over time
ggplot(transaction_by_day, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Filter to December and Look at individual days
ggplot(transaction_by_day[month(DATE) == 12, ], aes(x = DATE, y = N)) + 
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

#### Always check your output
#### Let's check if the pack sizes look sensible
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

transactionData

#### Let's plot a histogram of PACK_SIZE since we know that it is a categorical variable and not a continuous variable even though it is numeric.
hist(transactionData[, PACK_SIZE])

#### Brands
transactionData[, BRAND := toupper(substr(PROD_NAME, 1, regexpr(pattern = ' ', PROD_NAME) - 1))]

#### Checking brands
transactionData[, .N, by = BRAND][order(-N)]

#### Clean brand names
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
transactionData[BRAND == "NCC", BRAND := "NATURAL"]
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]
transactionData[BRAND == "GRAIN", BRAND := "GRNWVES"]

#### Check again
transactionData[, .N, by = BRAND][order(BRAND)]


#### Examining customer data
str(customerData)
summary(customerData)

#### Examining the values of lifestage and premium_customer
customerData[, .N, by = LIFESTAGE][order(-N)]
customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]

#### Merge transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)

data[is.null(LIFESTAGE), .N]
data[is.null(PREMIUM_CUSTOMER), .N]

#### Data exploration is now Complete

#### Calculating total sales
#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

#### Create plot
p <- ggplot(data=sales) +
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customer <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]

#### Create plot
q <- ggplot(data = customer) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Plot and label with propotion of customers
q + geom_text(data = ggplot_build(q)$data[[1]], aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]

#### Create plot
ggplot(data = avg_units, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Perform an independent t-test between mainstream vs premium and budget midage and
#### young singles and couples
pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER == "Mainstream", price]
       , data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
              & PREMIUM_CUSTOMER != "Mainstream", price]
       , alternative = "greater")

#### Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"),]

#### Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]

quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = BRAND]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = BRAND]

brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]

#### Preferred pack size compared to the rest of the population
quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]

pack_proportion <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
pack_proportion[order(-affinityToPack)]
data

#### Dive into what brands sell this pack size
data[PACK_SIZE == 270, unique(PROD_NAME)]
