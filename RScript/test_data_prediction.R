#### probabilty pred for test data 

library(taRifx)
companies <- read.csv("company.csv", header=TRUE, stringsAsFactors = F)
companies$market = as.factor(companies$market)
companies$region = as.factor(companies$region)
companies$funding_rounds = as.numeric(companies$funding_rounds)
companies$funding_total_usd = destring(companies$funding_total_usd)
ind.year = which(is.na(companies$funding_total_usd) == TRUE)
companies = companies[-ind.year,]
ind.year = which(is.na(companies$founded_year) == TRUE | companies$founded_year == "")
companies = companies[-ind.year,]
ind.year = which(companies$founded_year < 2015 & companies$founded_year > 1995)
companies = companies[ind.year,]
companies$founded_year = as.factor(companies$founded_year)
# train data is company which is ipo,acquired or closed and test data is company which is operating
train.data <- companies[-which(companies$status == "operating"),c(2,4,5,9,11,15,6,7,10,12,16)]
test.data <- companies[which(companies$status == "operating"),c(2,4,5,9,11,15,6,7,10,12,16)]
train.data$rank[which(train.data$status == "acquired" | train.data$status == "ipo")] = 1
train.data$rank[which(train.data$status == "closed")] = 0

##logistic model
model.logit = glm(rank~ market+ region +funding_rounds+log(funding_total_usd)+founded_year, data = train.data, family = "binomial")
ind = which(test.data$market %in% train.data$market == TRUE)
test.data = test.data[ind,]
ind = which(test.data$region %in% train.data$region == TRUE)
test.data = test.data[ind,]
test.data$rankpred = predict(model.logit, digit = 2, newdata = test.data, type = "response")
test.data = test.data[order(test.data$funding_total_usd, decreasing = TRUE),]
test.data1 = test.data[c(1,3,9,10,7,11,12)]
#test.data1 = test.data1[order(test.data1$rankpred, decreasing = TRUE),]
write.csv(test.data1, file ="success_prob.csv")
