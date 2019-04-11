custdata <- read.table('custdata.tsv', header = TRUE, sep = '\t')
head(custdata)
summary(custdata)
dim(custdata)
sum(is.na(custdata))
missing_cols <- table(which(is.na(custdata), arr.ind = T)[, 2])
summary(custdata[, as.numeric(names(missing_cols))])
custdata$is.employed <- as.factor(
  ifelse(is.na(custdata$is.employed),
         "missing",
         ifelse(custdata$is.employed == T,
                "employed",
                "not employed")
  ))
summary(custdata$is.employed)
summary(custdata$income)
custdata[sample(1:nrow(custdata), 56), "income"] <- NA
summary(custdata$income)
mean_income <- mean(custdata$income, na.rm = TRUE)
custdata$income <- ifelse(is.na(custdata$income),
                          mean_income,
                          custdata$income)
summary(custdata$income)
summary(custdata[is.na(custdata$housing.type),
                 c("recent.move", "num.vehicles")])
custdata <- custdata[complete.cases(custdata),]
d <- custdata # we will use it later
dim(custdata)
suppressMessages(library(ggplot2))
suppressMessages(library(scales))

## log_income
ggplot(custdata, aes(x = income)) + 
  geom_density(fill = "purple", color = "white", alpha = 0.5) +
  scale_x_continuous(labels = dollar) + theme_minimal() 
ggplot(custdata, aes(x = income)) + 
  geom_density(fill = "purple", color = "white", alpha = 0.5) +
  scale_x_log10(breaks = c(100, 1000, 10000, 100000), labels = dollar) +
  xlab("log income") + theme_minimal()
ggplot(custdata, aes(x = income, fill = health.ins)) +
  geom_density(color = 'white', alpha = 0.6, size = 0.5) +
  theme_minimal() + scale_fill_brewer(palette = 'Set1') +
  scale_x_log10(breaks = c(100, 1000, 10000, 100000), labels = dollar)
ggplot(custdata, aes(x = income, y = as.numeric(health.ins))) +
  geom_point() + geom_smooth() + scale_x_log10(labels = dollar) +
  theme_minimal()
  
binarized_income <- ifelse(custdata$income >= 30000,
                           ">=30K",
                           "<30K")
table(binarized_income)
probs <- quantile(custdata$age, probs = seq(0.1, 1, 0.1), na.rm = T)
age_groups <-
  cut(custdata$age,
      breaks = probs, include.lowest = T)
summary(age_groups)
range_normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

custdata[, "range_normalized_age"] <- range_normalize(custdata$age)
summary(custdata$range_normalized_age)
summary(custdata$age)
custdata$normalized_age <- custdata$age / mean(custdata$age)
summary(custdata$normalized_age)
custdata$age_gps <- factor(ifelse(custdata$normalized_age >= 1, "old", "young"))
ggplot(custdata, aes(x = age, fill = age_gps)) + 
  geom_density(color = "white", alpha = 0.5) + theme_minimal() +
  scale_fill_brewer(palette = 'Set1')
suppressMessages(library(dplyr))
table(custdata$state.of.res)

median_income <- custdata %>% group_by(state.of.res) %>%
  summarise(state_median_income = median(income, na.rm = TRUE))

dim(median_income)

custdata <- merge(custdata, median_income,
                     by = "state.of.res")

custdata$normalized_income <- with(custdata, income / state_median_income)
summary(custdata$normalized_income)

custdata$scaled_age <- (custdata$age - mean(custdata$age)) / sd(custdata$age)
summary(custdata$scaled_age)
### one-hot encoding
vars <- colnames(custdata)

## to one hot encode factor values and normalize numeric ones if needed
cat_vars <- vars[sapply(custdata[, vars], class) %in%
                   c("factor", "character", "logical")]
cat_vars <- cat_vars[-1] #state of res
custdata2 <- custdata[, cat_vars]
for (i in cat_vars) {
  dict <- unique(custdata2[, i])
  for (key in dict) {
    custdata2[[paste0(i, "_", key)]] <- 1.0 * (custdata2[, i] == key)
  }
}
# to remove the original categorical variables
#custdata[, which(colnames(custdata) %in% cat_vars)] <- NULL
head(custdata2)
table(custdata$marital.stat) #predictor
table(custdata$health.ins) #outcome
with(custdata, table(marital.stat, health.ins))

odds_ratio <- function(x, y, pos, logarithm = FALSE) {
  prob_table <- table(as.factor(y), x)
  vals <- unique(y)
  neg <- vals[which(vals != pos)]
  outcome_prob <- sum(y == pos, na.rm = T) / length(y)
  odds_ratio <-
  (prob_table[pos,] + 0.001) / (prob_table[neg, ] + 0.001)
  odds <- odds_ratio[x]
  odds[is.na(odds)] <- outcome_prob
  if (logarithm) {
  odds <- log(odds)
  }
  odds
}

vars <- colnames(custdata)
custdata3 <- custdata
cat_vars <- vars[sapply(custdata[, vars], class) %in% 
                   c("factor", "character", "logical")]
cat_vars <- cat_vars[which(cat_vars != "health.ins")] 
custdata3[, cat_vars] <- apply(custdata3[, cat_vars], 2,
                              function(x, y, pos) {
                              odds_ratio(x, as.factor(custdata3$health.ins),
                              "TRUE")
                              })
head(custdata3)

## Single Variable Model Function
single_variable_model <- function(x, y, pos) {
  if (class(x) %in% c("numeric", "integer")) {
    # if numeric descretize it
    probs <- unique(quantile(x, probs = seq(0.1, 1, 0.1), na.rm = T))
    x <- cut(x, breaks = probs, include.lowest = T)
  }
  prob_table <- table(as.factor(y), x)
  vals <- unique(y)
  neg <- vals[which(vals != pos)]
  outcome_prob <- sum(y == pos, na.rm = T) / length(y) #outcome probability
  cond_prob <-
    (prob_table[pos,] + 0.001 * outcome_prob) /
    (colSums(prob_table) + 0.001) # probability of outcome given variable
  cond_prob_model <- cond_prob[x]
  cond_prob_model[is.na(cond_prob_model)] <- outcome_prob
  cond_prob_model
}

head(d) #original custdata
set.seed(13)
d$split <- runif(nrow(d))
train <- subset(d, split <= 0.9)
test <- subset(d, split > 0.9)
train$split <- test$split <- NULL

suppressMessages(library(ROCR))
auc <- function(model, outcome, pos) { 
  per <- performance(prediction(model, outcome == pos),
                     "auc")
  as.numeric(per@y.values)
}

vars <- setdiff(colnames(train), c("custid", "health.ins"))
train[, paste0("svm_", vars)] <- apply(train[, vars], 2,
                                       function(x, y, pos) {
                                       single_variable_model(x, as.factor(train$health.ins),
                                       "TRUE")
                                       })
test[, paste0("svm_", vars)] <- apply(test[, vars], 2,
                                      function(x, y, pos) {
                                        single_variable_model(x, as.factor(test$health.ins),
                                                              "TRUE")
                                      })

head(train)
head(test)

svm_vars <- grep("svm_", colnames(train), value = TRUE)
for(i in svm_vars){
  train_svm_auc <- auc(train[, i], train$health.ins, "TRUE")
  print(sprintf("%s: train_AUC: %4.3f",
                i, train_svm_auc))
}

loglikelihood <- function(y, py) {
  sum(y * log(py) + (1 - y) * log(1 - py))
}

outcome <- "health.ins"
pos <- "TRUE"

pnull <- sum(train[, outcome] == pos) / length(train[, outcome])
null_deviance <- -2 * loglikelihood(as.numeric(train$health.ins), pnull)

sv_model_deviance <- c()
variable <- c()
for(i in svm_vars){
  sv_model_deviance <- c(sv_model_deviance,
                         -2 * loglikelihood(as.numeric(train$health.ins), 
                                            train[, i]))
  variable <- c(variable, i)
}
names(sv_model_deviance) <- variable
sv_model_deviance
deviance_improvement <- null_deviance - sv_model_deviance
sort(deviance_improvement, decreasing = TRUE)
sv_pseudo_rsquared <- 1 - (sv_model_deviance / null_deviance)
sort(sv_pseudo_rsquared, decreasing = TRUE)
selected_vars <- names(sv_pseudo_rsquared)[which(sv_pseudo_rsquared >= 0.1)]
selected_vars

glm_model <- glm(health.ins ~ .,
                 data = train[, c("health.ins", selected_vars)],
                 family = binomial(link = "logit"))
summary(glm_model)

train$glm_model <- predict(glm_model, train,
                           type = "response")
auc(train$glm_model, train$health.ins, "TRUE")
suppressMessages(library(WVPlots))
WVPlots::ROCPlot(train, 
                 "glm_model", outcome, pos,
                 'train performance')

test$glm_model <- predict(glm_model, test,
                          type = "response")
auc(test$glm_model, test$health.ins, "TRUE")
WVPlots::ROCPlot(test, 
                 "glm_model", outcome, pos,
                 'test performance')
