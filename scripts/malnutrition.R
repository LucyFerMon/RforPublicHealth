
#################################################################################
####### Evaluating malnutrition in Malawi at district level #####################
#################################################################################


#### 0) Importing data from DHIS-2 in .xslx format ####
malnutrition_U5yrs <- read_excel("~/Dropbox/Job documentation/WMO/Malawi/data/Nutrition/malnutrition_U5yrs.xlsx")

### We don't like keeping uncessary information, so we delete all the NA lines..whic
### are many
malnutrition_U5yrs <- malnutrition_U5yrs[-c(97:122),]

#### 1) Which is the district showing more cases in absolute numbers? ####

## 1.1 We accumulate the cases per district
summary_districts <- apply(malnutrition_U5yrs[3:ncol(malnutrition_U5yrs)] , 2, FUN=sum, na.rm=TRUE)

## 1.2 We order this data set from max to min to see which districts present the 
## greatest number of cases
sort(summary_districts, decreasing=TRUE)

#### 2) Calculate the malnutrition rates per district (for 100.000 people)  ####
#### so we need population data:
population <- read_csv("~/Dropbox/Job documentation/WMO/Malawi/data/ObservationData_pop_U5.csv")

### This data set has way to many columns and we just need the MOST RECENT total number
### of children under 5 for each district. 
### We explore the columns to see where is the information of our interest
## Districts
unique(population$`Region Name`)
## We see we can delete some metadata extra lines that we have

population <- population[ -c(28:30),]

### We selectthe columns that have the numbers we want, and leave out the rest of the
## unecessary information to minimize noise.

population_U5 <- population %>%
        select( `Region Name`, `2008`)

### now we divide the cases by the population. For that, the numbers of the districts need to match
### if they don't match, we alread now a trick to solve it ;). 

### Visualize the names
names_in_malnutrition <- sort(colnames(malnutrition_U5yrs))
names_in_population <- sort(unique(population$`Region Name`))

### Ask them is the match
which(!names_in_malnutrition  %in% names_in_population)

### OPS none of them match!
### Pay attention to the names, on the malnutrition data set they happend to have 
### an space after the name. Our function trimws() will solve it (run ?trimws for more info)

### We fix the column names
colnames(malnutrition_U5yrs) <- trimws(colnames(malnutrition_U5yrs))

### We resign the new colum names
names_in_malnutrition <- sort(colnames(malnutrition_U5yrs))

### We run our query again:
which(!names_in_malnutrition  %in% names_in_population)

### OPS! Two lines don't match, which ones?
lines_no_match <- which(!names_in_malnutrition  %in% names_in_population)

### We visualize them
names_in_malnutrition[lines_no_match]

### and we see they are the ones with the month and the year but for one district for 
### which there is no census data. As the district names match,  we have just saved
### a tone of work of aligning names, etc...!

### Now! we need to put together our malnutrition data with our district data 
### to be able to divide the number of cases by the number of children under 5
### but the two data sets look pretty different...look at them!

### The population data set has the all the district names in one column
### The malnutrition data sets uses one column for each district. 
### Althouhg a bit more ugly, a data form where district names are all in one column
### and cases in a separate column, is prefered, this is what is called: long format. 

### we convert from wide format (one column per district) to long format (all districts in one colum)
### with a very useful function of the package tidyr

malnutrition_U5yrs_long <- gather(malnutrition_U5yrs, key=district, value=cases, Balaka:Zomba)

### This one in fact looks REEEEEEAAALLLY long! Reflecting its name "long format".

### Now we can bind both datasets together. We will MERGE them, using the names of 
### the districts as the common information. But!  in order to use the district name
## as a common information we need to have one column in each dataset, called the same
### eg. "district". We already have it in our long format, but we don't have it in our population
### format. We can do this with our beloved dplyr function "rename"

population_U5 <- rename(population_U5, district=`Region Name` )

### And now, yes! We can merge it using our "district" common column
malnutrition_U5 <- merge(malnutrition_U5yrs_long, population_U5, by="district")

### But wait, now the column with the population numbers is called "2008", how weird!
### We rename it

malnutrition_U5 <-rename(malnutrition_U5, pop=`2008`)

### Now we can calculate the cases per number of children under 5. We make a new column for that

malnutrition_U5 <- malnutrition_U5 %>%
        mutate(malnutrition_rate = cases/pop)
### As the rate is sooo low! Let's make it by 100.000 people 

malnutrition_U5 <- malnutrition_U5 %>%
        mutate(malnutrition_rate = 100000*cases/pop)


#### 3) Plot the cases over time to see which districts are improving and which going worse..
### We learnt how to work with dates already

### We create an artificial date asigning first day of the month to each month so that
### we can easily make beautiful plots in with ggplot
malnutrition_U5$date <-  paste(1, malnutrition_U5$MONTH, malnutrition_U5$YEAR, sep="-")
malnutrition_U5$date <- tolower(malnutrition_U5$date)
malnutrition_U5$date <- as.Date(malnutrition_U5$date, format="%d-%B-%Y")

### We plot our data now indicating that we want to see the month-year in the 
### x axis
ggplot(malnutrition_U5, aes(y=cases, x=date)) +
        geom_line() +
        scale_x_date(date_labels = "%b-%y")+
        facet_grid(district~.) 

### This is a whole load of districts for onw plot. We could divide by region
### or plot district by district.

#### 3) Calculate the seasonality in malnutrition cases per district ####

### BOXPLOT VIEW: 

malnutrition_U5$MONTH <- factor(malnutrition_U5$MONTH, levels=c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE","JULY", "JULY",  "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"))


ggplot(malnutrition_U5[malnutrition_U5$district=="Balaka",], aes(y=cases, x=MONTH)) +
        geom_boxplot()+
        theme_bw() +
        xlab("Month") +
        ylab("cases / 100.000")+
        ggtitle("Seasonality of malnutrition, calculated between 2004 and 2012, for the district Balaka")


ggplot(malnutrition_U5, aes(y=cases, x=MONTH)) +
        geom_boxplot() +
        facet_grid(district~.) 

#### 4) Calculate the yearly trend of malnutrition per district ####

### We use the same method to calculate the yearly trend of malnutrition in each of the districts.

malnutrition_U5$YEAR <- as.factor(malnutrition_U5$YEAR)

ggplot(malnutrition_U5[malnutrition_U5$district=="Balaka",], aes(y=cases, x=YEAR)) +
        geom_boxplot()+
        theme_bw() +
        xlab("Month") +
        ylab("cases / 100.000")+
        ggtitle("Trend of malnutrition, calculated between 2004 and 2012, for the district Balaka")



