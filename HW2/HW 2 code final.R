library(treemap); library(lubridate); library(lattice); library(MASS); library(stringr)
library(tidyverse); library(data.table); library(dplyr); library(plyr); library(maps)
library(ggplot2) # libraries

setwd("C:/Users/qizhe/Desktop/STA 141A/HW2") # set working directory

house <- readRDS("housing.rds") # give a name to the data

# 1
# change variables to proper types
str(house)
house$zip <- as.factor(house$zip)
house$year <- as.numeric(house$year)

# Fix county to County using stringr's str_replace()
table(house$county) # lowercase counties
house$county <- gsub(house$county, pattern = "county", replacement = "County") # fix county to County
house$county <- gsub(house$county, pattern = "Franciscoe", replacement = "Francisco") # fix Franciscoe to Francisco

# possible values to plot: zip, price, br, lsqft, bsqft, year, date, long, lat
# zip
max(as.numeric(house$zip), na.rm = T) # all are within the required 9---- 
min(as.numeric(house$zip), na.rm = T)
table(house$zip)
any(sapply(house$zip, function(x) nchar(as.character(x))) != 5, na.rm=TRUE) # no zip code not in 5 digits

# price
hist(house$price, xlab = "House Price", main = "Histogram of Home Prices")
boxplot(house$price, ylab = "House Price", main = "Boxplot of Home Prices")
max(house$price, na.rm = T)
sort(house$price, decreasing = T)[1:10] # the maximum is not too far from the rest of the data
sort(house$price, decreasing = F)[1:10] # certain values are 0, so they should be set to NA
house <- house %>% # set unlikely value to NA
  mutate(price = ifelse(price == 0, NA, price))
house[which(house$price == 7000000),]
# https://www.zillow.com/homedetails/16-Skyland-Way-Ross-CA-94957/19273018_zpid/
# Some information such as price seem accurate, however other information is different than
# what is listed. The owner can easily alter the home over time so these other differences are ignored.
house[which(house$price == 5574000),]
# https://www.zillow.com/homedetails/138-Gilmartin-Dr-Tiburon-CA-94920/19261746_zpid/
# According to the website, the price seems accurate
house[which(house$price == 5434000),]
# https://www.zillow.com/homedetails/11-Southwood-Ave-Ross-CA-94957/19273220_zpid/
# Again the information seems accurate
house[which(house$price == 5400000),]
# https://www.redfin.com/CA/Mill-Valley/43-Park-Ave-94941/home/911182
# This price actually seems incorrect according to this listing
house <- house %>% # set unlikely value to NA
  mutate(price = ifelse(price == 5400000, NA, price))

# br
hist(house$br, xlab = "Bedrooms", main = "Number of Bedrooms")
max(house$br, na.rm = T)
sort(house$br, decreasing = T)[1:10] # one is considerably larger than the others
sort(house$br, decreasing = F)[1:10]
house[which(house$br == 28),]
# https://www.redfin.com/CA/Redwood-City/75-Duane-St-94062/home/1741841
# the website shows that it's an apartment complex
house[which(house$br == 16),]
# https://www.redfin.com/CA/Rohnert-Park/909-Kirsten-Ct-94928/home/2544629
# the website shows that it's only an 8br multi-family home
house[which(house$br == 12),]
# https://www.movoto.com/oakland-ca/8290-macarthur-blvd-oakland-ca-94605/pid_rrw8uaf38g/
# the website shows that one is a 12 br quadruplex, so these sizes are possible

# lsqft
hist(house$lsqft, xlab = "Lot Square feet", main = "Histogram of Home Lot Size")
boxplot(house$lsqft, main = "Boxplot Lot Size of Home")
max(house$lsqft, na.rm = T)
sort(house$lsqft, decreasing = T)[1:10]
sort(house$lsqft, decreasing = F)[1:10]
house[which(house$lsqft == 313632000),]
# https://www.zillow.com/homedetails/2118-Peppertree-Way-APT-2-Antioch-CA-94509/18316652_zpid/
# The property doesn't look so large, likely an error
house[which(house$lsqft == 65340000),]
# https://www.redfin.com/CA/Walnut-Creek/1904-Ptarmigan-Dr-94595/unit-1/home/888404
# The property doesn't look so large, likely an error
house[which(house$lsqft == 51399998),]
# https://www.trulia.com/p/ca/san-francisco/439-greenwich-st-9-san-francisco-ca-94133--2083094246
# The property is only on a small area of a street so there's an error
house[which(house$lsqft == 25),]
# https://www.zillow.com/homedetails/318-Shirley-St-Graton-CA-95444/15830352_zpid/
house <- house %>% # set unlikely value to NA
  mutate(lsqft = ifelse(street == "318 Shirley Street", NA, lsqft))
house[which(house$lsqft == 30),]
# https://www.redfin.com/CA/San-Francisco/39-Scott-St-94117/home/1349981
# Actually shown as 29 lsqft
house[which(house$lsqft == 100),]
# https://www.zillow.com/homedetails/648-Ridgewood-Ave-Mill-Valley-CA-94941/94645334_zpid/
# Shown to be an error
house <- house %>% # set unlikely value to NA
  mutate(lsqft = ifelse(street == "648 Ridgewood Avenue", NA, lsqft))

# bsqft
hist(house$bsqft, xlab = "Building Size in Sqft.", main = "Histogram of Building Size")
boxplot(house$bsqft, ylab = "Building Size in Sqft.", main = "Boxplot of Building Size")
max(house$bsqft, na.rm = T)
sort(house$bsqft, decreasing = T)[1:10]
sort(house$bsqft, decreasing = F)[1:10]
house[which(house$bsqft == 22266),]
# Same as before, it's an apartment complex
house[which(house$bsqft == 14149),]
# https://www.redfin.com/CA/San-Francisco/1010-Gough-St-94109/home/979662
# The website show's that it's a multi-family home with this actual bsqft
house[which(house$bsqft == 12582),]
# https://www.redfin.com/CA/Oakland/1009-E-22nd-St-94606/home/1398101
# The link shows that it's only 2,582 bsqft, there's an extra '1' added
house <- house %>% # set unlikely value to NA
  mutate(bsqft = ifelse(bsqft == 12582, NA, bsqft))
house[which(house$bsqft == 11064),]
# Same as before, it's a multi-family home with correct bsqft
house[which(house$bsqft == 10898),]
# https://www.redfin.com/CA/Oakland/4609-Rising-Hill-Ct-94619/home/1820925
# The link also has the same information
house[which(house$bsqft == 370),]
# No information on bsqft online
house[which(house$bsqft == 380),]
# https://www.zillow.com/homedetails/579-Beresford-Ave-Redwood-City-CA-94061/58660857_zpid/
# Information online is quite different
house[which(house$bsqft == 381),]
# https://www.zillow.com/homedetails/1412-Wisner-Dr-Antioch-CA-94509/18305299_zpid/
# Very different information online

# year
class(house$year) # character type, must change to workable format
options(warn=-1) # prevent warnings
# https://stackoverflow.com/questions/15680350/plot-a-histogram-without-zero-values-in-r
table(is.na(house$year)) # over 17% of the observations have an NA for year (3507 NA's)
# https://stackoverflow.com/questions/16194212/how-to-suppress-warnings-globally-in-an-r-script
options(warn=0) # turn warnings back on
sort(house$year, decreasing = T)[1:10] # Several impossible values at the top
sort(house$year, decreasing = F)[1:20] # Several impossible values at the bottom
house <- house %>% # set impossible year values to NA
  mutate(year = ifelse(year < 1800 | year > 2020, NA, year))
table(is.na(house$year)) # 3526 NA's, 19 impossible years

# date
sort(year(house$date))[1:10] # no irregular years/months/days
sort(year(house$date), decreasing = T)[1:10]
sort(month(house$date))[1:10]
sort(month(house$date), decreasing = T)[1:10]
sort(day(house$date))[1:10]
sort(day(house$date), decreasing = T)[1:10]

plot(house$date, xlab = "Date Sold", ylab = "Frequency", main = "Date of Home Sales")
# pattern of houses not being sold around the beginning of the year

# long
boxplot(house$long, ylab = "Longitude of Home", main = "Boxplot of Longitudes") # boxplot
sort(house$long, decreasing = T)[1:10] # examine extreme values
sort(house$long, decreasing = F)[1:10]
house[which(house$long == 0),] # identify the outlier
house <- house %>% # set the outlier to NA
  mutate(long = ifelse(long > -120, NA, long))
boxplot(house$long, ylab = "Longitude of Home", main = "Boxplot of Longitudes") # data now apppears more regular

# lat
boxplot(house$lat, ylab = "Latitude of Home", main = "Boxplot of Latitudes") # boxplot
sort(house$lat) # examine extreme values
house[which(house$lat == 0),] # same observation as previously
house <- house %>% # set the outlier to NA
  mutate(lat = ifelse(lat < 30, NA, lat))
boxplot(house$lat, ylab = "Latitude of Home", main = "Boxplot of Latitudes") # data now apperas more regular

# duplication
sum(duplicated(house)) # there are duplications in the data
house[duplicated(house),]
house[house$street == "1882 48th Avenue", ]
house = house[-which(duplicated(house)),] # remove duplciation 

# 2
sort(house$date)[1:10] # starts at 4/27/03
sort(house$date, decreasing = T)[1:10] # ends at 6/4/06
first_sale <- ymd("2003-04-27") # convert both to units that can be subtracted
last_sale <- ymd("2006-06-04")
last_sale - first_sale # 1134 days difference

class(house$year)
sort(house$year)[1:10] # starts in 1923
sort(house$year, decreasing = T)[1:10] # ends at 2005
2005-1885 # 82 years of home construction

# 3
# order dates
house$date_ym <- format.Date(house$date, "%Y-%m") # formatted date
house$year <- strptime(house$year, "%Y")

date_table <- table(house$date_ym) # table the month-year data
avg_price <- aggregate(price ~ date_ym, house, mean, na.rm = T) # take the average of the prices
avg_price$ym <- paste(avg_price$date_ym, "-01") # include an extra day variable to signify the first of the month

date_aggregate <- as.Date(avg_price$ym, "%Y-%m -%d") # change to Date class

plot(x=parse_date_time(names(date_table),"y-m"),y=date_table, type = 'b',
     xlab = "Date of Sales", ylab = "Number of Sales", main = "Sales per Month") # number of sales
plot(date_aggregate, avg_price$price, type = "l", xlab = "Date of Sales",
     ylab = "Average Price($)", main = "Average Price per Month") # average price

# 4
table(as.factor(house$br)) # check bedrooms
house$br <- as.integer(house$br) # change to integer

house <- house %>% # set Alpine County cities to San Francisco (from problem 5)
  mutate(county = ifelse(county == "Alpine County", "San Francisco County", county))

house$sale_Year <- year(house$date) # subset sale years 2003 - 2005
house05 <- subset(house, sale_Year < 2006) # remove years less beyond 2005
house05$Bedrooms <-cut(house05$br, breaks = c(0,1,2,3,Inf), 
                       labels = c("1", "2", "3", "4+")) # set bedroom levels
br_avg <- aggregate(price ~ county + Bedrooms + sale_Year, house05, mean, na.rm = T) # take average price
xticks = c(2003, 2004, 2005) # fix x-labels
# http://ggplot2.tidyverse.org/reference/labs.html
ggplot(br_avg, aes(x = sale_Year, y = price, col = Bedrooms)) + geom_line() + # plot the counties
  facet_wrap(~ county) + scale_x_continuous(breaks = xticks) + 
  labs(x = "Sale Year", y = "Price") + labs(title = "Housing Price by Number of Bedrooms")

# 5
table(house$city, house$county) # draw a table of city/county table(city,county)
which(rowSums(table(unique(house[c("city","county")])))>1) # duplicates in Vallejo, San Francisco

# https://www.statmethods.net/management/subset.html
alpsf <- subset(house, city == "San Francisco" & county == "Alpine County") # examine the Alpine County data
# 19 observations that have this mistake
vall <- subset(house, city == "Vallejo") # examine the Vallejo data
# 2 errors in vallejo/napa/solano
# https://www.redfin.com/CA/American-Canyon/28-Spikerush-Cir-94503/home/12222859
# http://www.loopnet.com/Listing/15529307/3860-Broadway-Drive-American-Canyon-CA/
# Belvedere/tirburon, belvedere/Tiburon (mislabeled data)

# 6
plot(house$bsqft, house$price, xlab = "Building Size (sqft.)", # scatter plot of price and bsqft
     ylab = "Home Price($)", main = "Price vs. Building Size")
abline(lm(price ~ bsqft, data = house[-1])) # regression line
houselm <- lm(price ~ bsqft, data = house[-1]) # examine the summary
summary(houselm)
par(mfrow=c(2,2)) # allow for 2x2 plots
plot(lm(house$price ~ house$bsqft),id.n=10) # plot the lm diagnostic
# examine points 987, 19332
house[987,] # apartment complex
# https://www.redfin.com/CA/Redwood-City/75-Duane-St-94062/home/1741841
# the website shows that it's an apartment complex
house[19332,]
# https://www.redfin.com/CA/Oakland/1009-E-22nd-St-94606/home/1398101
# The link shows that it's only 2,582 bsqft, there's an extra '1' added
house <- house %>% # set unlikely value to NA
  mutate(bsqft = ifelse(bsqft == 12582, NA, bsqft))
dev.off()

house_normal <- subset(house, bsqft < 10000 & price < 4000000) # subset data according to bsqft and price
house_normal$pos_price <- ifelse(house_normal$price <= 0, NA, house_normal$price) # set 0's to NA for log()
plot(log(house_normal$bsqft), log(house_normal$pos_price), xlab = "log Building Size (sqft.)", # scatterplot of log data
     ylab = "log Home Price($)", main = "Price vs. Building Size (log scale)")
abline(lm(log(price + 1) ~ log(bsqft), data = house_normal[-1])) # regression line
houselm2 <- lm(log(price + 1) ~ log(bsqft), data = house_normal[-1]) # examine summary
summary(houselm2)
par(mfrow=c(2,2)) # allow for 2x2 plots
plot(lm(log(house_normal$pos_price) ~ log(house_normal$bsqft)), id.n = 10, main = "(log scale)") # plot the lm diagnostic
dev.off()

# 7
# H_0
# B_bsqft >= B_lsqft
# B_bsqft - B_lsqft >= 0
# H_1
# B_bsqft - B_lsqft < 0
# let B = B_bsqft - B_lsqft
# So H_0 is B >= 0
# and H_1 is B < 0
# conclusion and test statistic
# test statistic:
# B - 0 / sqrt(Var(B))
# sqrt(Var(B)) = sqrt(Var(B_bsqft - B_lsqft)) = sqrt(Var(B_bsqft) + Var(B_lsqft))
summary(lm(price~bsqft+lsqft,house))$coefficients # examine the std. errors and estimates ofr both variables

# 8
split_counties <- split(house, house$county) # separate data according to county
lm_split_counties <- lapply(split_counties, function(x) lm(price ~ bsqft, data = x)) # linear model for each county
plot(house$bsqft, house$price, xlab = "Building Size (sqft.)", # plot building size against prices
     ylab = "Home Price($)", main = "Price vs. Building Size")
sapply(1:9, function(x) abline(coef(lm_split_counties[[x]]), col = x, lty = c(1:6,1:3)[x] )) # draw reg. line per county
legend("bottomright", names(table(house$county)), col = 1:9, lty = rep_len(1:6, 9)) # include legend of lines

# 9
house$county <- as.factor(house$county) # change into factor levels
sales_per_city <- t(t(table(house$city))) # get the count of sales per city
sales_per_city <- as.data.frame(sales_per_city) # make the data into a useable format
sales_per_city <- sales_per_city[,-2] # remove unecessary column

# https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
colnames(sales_per_city) <- c("city", "sales") # add labels

house$county <- as.character(house$county)
cities <- house[,c(1,2)] # create new dataframe with cities and county

# http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
cities <- arrange(cities,city) # order by city

sales_per_city <- sales_per_city %>% # combine both through joins
  left_join(cities, by = "city")

# https://stackoverflow.com/questions/13967063/remove-duplicated-rows
sales_per_city <- sales_per_city[!duplicated(sales_per_city),] # drop duplicates

# https://stackoverflow.com/questions/27766054/getting-the-top-values-by-group
sales_per_city <- sales_per_city %>% # subset the top 3 per county
  group_by(county) %>%
  top_n(n = 3, wt = sales)

sales_per_city <- sales_per_city %>% # orders counties alphabetically and according to highest price
  group_by(sales) %>%
  arrange(county)

avg_city <- aggregate(price ~ city, house, mean, na.rm = T) # average price per city

sales_per_city <- sales_per_city %>% # combine both through joins
  left_join(avg_city, by = "city")

colnames(sales_per_city) <- c("City", "Sales", "County", "Average Price") # add labels

# https://bookdown.org/lyzhang10/lzhang_r_tips_book/how-to-plot-data.html#creating-treemaps
# https://www.r-graph-gallery.com/236-custom-your-treemap/
treemap(dtf = sales_per_city, # create treemap 
        index = c("County", "City"), 
        vSize = "Sales", # number of sales
        vColor = "Average Price", # average price
        type = "value", 
        palette = "Spectral", 
        border.col = c("grey70", "grey90"),
        fontsize.labels = c(10, 8), 
        title = "Treemap of Top 3 Cities by County")

# 10
# 1. A heatmap that is colorized based on the number of houses in a cell
sfhouse <- subset(house, city == "San Francisco") # subset SF
sfhouse$long2 <- round(sfhouse$long, 2) # round values
sfhouse$lat2 <- round(sfhouse$lat, 2)
long_range<-range(sfhouse$long2,na.rm=TRUE) # create overall range for longitude/latitude
lat_range<-range(sfhouse$lat2,na.rm=TRUE)
sfhouse$longF <- factor(sfhouse$long2,levels=seq(long_range[1],long_range[2],.01)) # create factor levels
sfhouse$latF <- factor(sfhouse$lat2,levels=seq(lat_range[1],lat_range[2],.01))
sf_matrix <- table(sfhouse$longF,sfhouse$latF) # create table matrix of longitude/latitude data
sf_matrix[sf_matrix == 0] <- NA # set 0's to NA (Patrick's advice)
image(x = as.numeric(levels(sfhouse$longF)) + .03, y = as.numeric(levels(sfhouse$latF)), z = sf_matrix, #create heatmap
      xlab = "Longitude", ylab = "Latitude", main = "San Francisco Heatmap of Home Locations")
CA_boarder=map('county',plot=FALSE); lines(CA_boarder$x,CA_boarder$y) # draw border of San Francisco

# 2. A heatmap that is colorized by the average price of the houses within a cell 
longlat_avg <- aggregate(price ~ longF + latF, sfhouse, mean, na.rm = T) # average per longitude/latitude interval
# https://stackoverflow.com/questions/9617348/reshape-three-column-data-frame-to-matrix-long-to-wide-format
sf_avg_map <- daply(longlat_avg, .(longF,latF), function(x) x$price) # set data into proper matrix
image(x = as.numeric(levels(sfhouse$longF)) + .03, y = as.numeric(levels(sfhouse$latF)), z = sf_avg_map, #create heatmap
      xlab = "Longitude", ylab = "Latitude", main = "San Francisco Heatmap of Average Home Prices")
CA_boarder=map('county',plot=FALSE); lines(CA_boarder$x,CA_boarder$y) # draw border of San Francisco
