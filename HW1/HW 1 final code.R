setwd("C:/Users/qizhe/Desktop/STA 141A/HW1") # set working directory

cs <- readRDS("college_scorecard_2013.rds") # give a name to the data

# 1 Total of 3312 observations, 2431 different colleges.
dim(cs) # 3312 observations
# number of colleges
sum(cs$main_campus) #2431 main campuses

# 2
# examine the number of different classes of variables
tab1 <- sapply(cs, class)
table(tab1)

# examine each variable type using str()
str(cs)

# 3
sum(is.na(cs)) # number of NA's 23197

# find number of NA's per variable, find the largest
count_na_by_features = sapply(cs, function(x) sum(is.na(x)))
t(t(count_na_by_features)) # avg_sat is largest with 1,923

# graphically display the NA's per feature
NA_counts<-colSums(is.na(cs)) # save NA's to variable
plot(NA_counts, main="Number of Missing Data per Feature",
     xlab = "Feature Index",
     ylab = "Number NA's", cex = 1, pch=15)
abline(h=c(500, 0), col="Green") # many around this region

# 4
# number of private versus public colleges
length(which(cs$ownership=="Public")) # public 716
length(which(cs$ownership!="Public")) # private aka For Profit and Nonprofit 2,596

# separate public and private colleges into variables
cs_public = cs[cs$ownership=="Public",]
cs_private = cs[cs$ownership!="Public",]

# graphically display proportions of degrees for private and public colleges
public_highest_deg_table = table(cs_public$highest_degree)/sum(table(cs_public$highest_degree))
private_highest_deg_table = table(cs_private$highest_degree)/sum(table(cs_private$highest_degree))
par(mfrow=c(1,2))
barplot(public_highest_deg_table, main = "Degree Proportion in Public College", ylim=c(0,0.8))
barplot(private_highest_deg_table, main = "Degree Proportion in Private College", ylim=c(0,0.8))
dev.off() # reset frames

# 5
# mean, median, and decile for undergraduate populations
ugmean <- mean(cs$undergrad_pop,na.rm = TRUE) #3599.502
ugmed <- median(cs$undergrad_pop,na.rm = TRUE) #1295
decile <- quantile(cs$undergrad_pop, prob = seq(0,1,length=11),type = 5,na.rm = TRUE)
hist(cs$undergrad_pop, main = "Histogram of undergraduate population", xlab = "Undergraduate Population")
abline(v = ugmean, col='red', lwd=5)
abline(v = decile, col='pink', lwd=2)
abline(v = ugmed, col='blue', lwd=5)
legend(x=120000, y=2400, c("mean", "median", "decile"), col = c("red", "blue", "pink"), lty=c(1,1,1))
#There is an extreme outlier, the Universit of Phoenix, Arizona whose population of over
#150,000 students creates an extreme skew to the histogram.

# testing for without the previous outlier, little difference in overall skew
cs1 = cs[-which(cs$undergrad_pop==166816),] # removes outlier
ugmean <- mean(cs1$undergrad_pop,na.rm = TRUE) #3599.502
ugmed <- median(cs1$undergrad_pop,na.rm = TRUE) #1295
decile <- quantile(cs1$undergrad_pop, prob = seq(0,1,length=11),type = 5,na.rm = TRUE)
hist(cs1$undergrad_pop, main = "Histogram of undergraduate population")
abline(v = ugmean, col='red', lwd=5)
abline(v = decile, col='pink', lwd=2)
abline(v = ugmed, col='blue', lwd=5)
legend(x=120000, y=2400, c("mean", "median", "decile"), col = c("red", "blue", "pink"), lty=c(1,1,1))

# 6
# 5 most populous states: California, Texas, New York, Illinois, Florida
top = cs[cs$state %in% c("CA", "TX", "NY", "IL", "FL"),]
top$state <- droplevels(top$state)
boxplot(top$tuition~top$state, main = "Boxplot of 5 Most Populous States", ylab = "Tuition per State")

# 7
# part a
# name of university with largest avg_sat
cs[which.max(cs$avg_sat), 'name'] #California Institute of Technology

# part b
# largest university & open admissions
cs[which.max(cs$undergrad_pop), c('name', 'open_admissions')]

# part c
# zip code of smallest avg_family_inc for public schools
publicUni = subset(cs,ownership=="Public") # subset by public
publicUni$zip[which.min(publicUni$avg_family_inc)] # 11101

# part d
# also largest grad_pop (referring to part b)
cs[which.max(cs$undergrad_pop), 'grad_pop'] == max(cs$grad_pop, na.rm=TRUE) # checks if part b is the max grad pop

# 8
# subset For Profit & Bachelor primary degree schools
profbach = subset(cs, ownership=="For Profit" & primary_degree=="Bachelor")
money_per_student <- profbach[,c('revenue_per_student', 'spend_per_student')]

# part a
plot(money_per_student, main = "Revenue versus Spending per Student", xlab = "Revenue per student"
     , ylab = "Spending per student")
#There are a few outliers which may violate the assumptions of linearity.

# part b
# create new variable to use
profbach[is.na(profbach)] <- 0 # set NA's to 0
net_income <- (profbach$revenue_per_student - profbach$spend_per_student) # net income for colleges
total_net_income <- (net_income*(profbach$undergrad_pop + profbach$grad_pop)) # total net income
# order profbach, and then choose top 5
profbach_top = profbach[order(total_net_income, decreasing=TRUE),]
profbach_top5 = profbach_top[1:5,] #University of Phoenix-Online Campus, Ashford University,
#Capella University, Grand Canyon University, Kaplan University-Davenport Campus

# plot net income and number of students
# create total student population, net income, and total net income categories for sub8_top5
profbach_top5$student_pop <- profbach_top5$grad_pop + profbach_top5$undergrad_pop
profbach_top5$net_income <- (profbach_top5$revenue_per_student - profbach_top5$spend_per_student)
profbach_top5$total_net_income <- (profbach_top5$net_income*(profbach_top5$undergrad_pop + profbach_top5$grad_pop))
plot(profbach_top5$student_pop, profbach_top5$total_net_income, xlab = "Student population", 
     ylab = "Total income per student", main = "Top 5 Total Net Income for Colleges", 
     pch=1:5)
legend("topleft", profbach_top5$name, pch=1:5)

# 9
plot(cs[,c("avg_sat", "admission")], main = "Average SAT versus Admissions", xlab = "Average SAT",
     ylab = "Admissions")
#Around avg_sat = 1200, the admission begins to narrow.

# split according to avg_sat >= 1200 and admission <= 0.4
group <- ifelse(cs$avg_sat >= 1200 & cs$admission <= 0.4, "High_SAT", "Low_SAT")
group <- ifelse(is.na(group),"Low_SAT",group)
cs$group <- group
boxplot(cs$med_10yr_salary~cs$group, ylab = "Median 10 year salary", main = "Median Salary for High/Low SAT")

cs$race_aw <- cs$race_asian + cs$race_white # create asian and white variable
boxplot(cs$race_aw~group, ylab = "Asian and White Proportion", main = "Asian and White Combination for High/Low SAT")

cs$grad_proportion <- (cs$grad_pop/(cs$grad_pop + cs$undergrad_pop)) # create proportion variable
boxplot(cs$grad_proportion~group, ylab = "Graduate Student Proportion", 
        main = "SAT Scores According to Graduate Student Proportion")

lsat=subset(cs, cs$group=="Low_SAT") #low SAT's
lsat$grad_proportion <- (lsat$grad_pop/(lsat$grad_pop + lsat$undergrad_pop))
lsat[order(lsat$grad_proportion, decreasing = TRUE),][1:5,]

# part C
mosaicplot(~  group + open_admissions, data = cs, main = "Open Admissions vs. Group",
           xlab = "Group", ylab = "Open Admissions")
table(as.character(cs$group), as.character(cs$open_admissions))

# examine table of 'group' and 'open_admissions, also view the distribution'
subgroup = cs$group
subgroup[is.na(subgroup)] = "NA"
suboa = cs$open_admissions
suboa[is.na(suboa)] = "NA"
table(suboa,subgroup)
subas = cs$avg_sat
subas[is.na(subas)] = "NA"
hist(cs$avg_sat)

# part C, part B
mosaicplot(~  group + main_campus, data = cs, main = "Main Campus vs. Group",
           xlab = "Group", ylab = "Main Campus")
table(as.character(cs$group), as.character(cs$main_campus)) # High_SAT FALSE = 0

# part C, part C
mosaicplot(~  group + ownership, data = cs, main = "Ownership vs. Group",
           xlab = "Group", ylab = "Ownership")
table(as.character(cs$group), as.character(cs$ownership)) # High_SAT For Profit = 0

# part C, part D
branches_counts = ifelse(cs$branches == 1, "one branches", "more than one branches")
mosaicplot(~ group + branches_counts, data=cs, main = "Branches Count vs. Group",
           xlab = "Group", ylab = "Branches Counts")
cs$branches_counts <- branches_counts
table(as.character(cs$group), as.character(cs$branches_counts)) # High_SAT For Profit = 0

# 10
# part A
plot(cs$avg_family_inc, cs$avg_10yr_salary, main = "Avg. Family Income vs. Avg. 10 yr. Salary",
     xlab = "Avg. Family Income", ylab = "Avg. 10 yr. Salary")
abline(lm(avg_10yr_salary~avg_family_inc, data=cs))

outliers = cs[cs$avg_10yr_salary > 150000 & !is.na(cs$avg_10yr_salary) & !is.na(cs$avg_family_inc),]
dim(outliers) # 9 observations
outliers$name

# part B
# check the number of factor levels
levels(cs$highest_degree)

# optional R/Stats practice, test the new categorical variable's factor levels to see
# if there's an improvement of fit
plot(cs$avg_family_inc, cs$avg_10yr_salary, main = "Avg. Family Income vs. Avg. 10 yr. Salary",
     xlab = "Avg. Family Income", ylab = "Avg. 10 yr. Salary")
fit1 = lm(avg_10yr_salary~avg_family_inc + highest_degree, data=cs)
fit1$coef
abline(a = fit1$coef[1], b = fit1$coef[2], col="orange")
abline(a = fit1$coef[1]+fit1$coef[3], b = fit1$coef[2], lty=2, col="pink")
abline(a = fit1$coef[1]+fit1$coef[4], b = fit1$coef[2], lty=3, col="green")
abline(a = fit1$coef[1]+fit1$coef[5], b = fit1$coef[2], lty=4, col="blue")
abline(a = fit1$coef[1]+fit1$coef[6], b = fit1$coef[2], lty=5, col="red")
abline(lm(avg_10yr_salary~avg_family_inc, data=cs))
legend("topright", c("other", "Certificate", "Associate", "Bachelor", "Graduate",
                     "Original regression line"),
       col = c("orange", "pink", "green", "blue", "red", "black"), lty = c(1,2,3,4,5,1))