require(dplyr)
require(lubridate)
require(boxr)
require(RCurl)
require(devtools)
require(car)
require(nzr, lib.loc = "C:/Program Files/R/R-3.1.3/library")
require(zoo)
require(caret)
require(ranger)
require(e1071)

## connect to AT&T db & run query below:
nzConnect("drew", "Kiopses1", "192.168.100.5", "CLIENT_ATT", force = TRUE)



### setup dataset for model with response est ga.

att_master_2 <- paste("select event_date, week_code, 
(case when week_code in (1,7) then 1 else 0 end) as weekend,
(case when week_code not in (1,7) then 1 else 0 end) as weekday,
sum(a.spend) as spend, sum(a.dr_spend) as dr_spend, sum(upper_spend) as upper_spend, sum(payable_orders) as payable_orders,
sum(a.impressions) as impressions, sum(a.clicks) as clicks
from
(
select event_date, funnel, extract(dow from event_date) as week_code,
sum(impressions) as impressions, sum(clicks) as clicks, sum(total_net_spend) as spend,
(case when funnel = 'D' then sum(total_net_spend) else 0 end) as dr_spend,
(case when funnel <> 'D' then sum(total_net_spend) else 0 end) as upper_spend,
(sum(PLAN_30) + sum(PLAN_35) + sum(PLAN_45) + sum(PLAN_65)) as payable_orders,
sum(IMM_BILLABLE_INITIAL) as initial_billing, sum(IMM_BILLABLE_EST) as est_billing 
from CLIENT_ATT.ARACINE.ATT_ADFORM_MASTER
where event_date >= '2018-03-01' and event_date < '2018-06-25'
group by 1,2,3
) a
group by 1,2,3,4
order by 1 desc")

ncol(att_mv)
                      
att_mv <- nzQuery(att_master_2)


## clean df mv
col_names_int <- c(3:10)
att_mv[,col_names_int] <- lapply(att_mv[,col_names_int], as.integer)
att_mv$EVENT_DATE <- as.Date(att_mv$EVENT_DATE)



### generate est payable response, dr percentage variables
att_mv <- att_mv %>%
  mutate(est_ga = PAYABLE_ORDERS * 0.32) %>%
  mutate(DR_PERC = DR_SPEND/SPEND) %>%
  filter(SPEND < 850) %>%
  filter(est_ga < 15) %>%
  filter(est_ga > 2)

plot(att_mv$est_ga)
nrow(att_mv)

### smooth ga data with 7 day moving avg
sma_ga_7 <- data.frame(rollmean(att_mv$est_ga, 7))
sma_spend_7 <- data.frame(rollmean(att_mv$SPEND, 7))
att_data <- head(att_mv, -6)
colnames(sma_ga_7) <- c("sma_ga_7")
colnames(sma_spend_7) <- c("sma_spend_7")
att_data <- cbind(att_data, sma_ga_7, sma_spend_7)


nrow(att_data)

hist(att_data$SPEND)

att_lm_data <- att_data %>%
  select(est_ga, SPEND, DR_PERC, WEEKDAY)


cor(att_lm_data, use = "complete.obs", method = "pearson")

### set same random split
set.seed(42)

### shuffle row
rows <- sample(nrow(att_lm_data))

### randomly order data
att_lm_data <- att_lm_data[rows, ]

### split data in 80/20
split <- round(nrow(att_lm_data) * .80)

### create train/test data
train <- att_lm_data[1:split, ]
test <- test <- att_lm_data[(split + 1):nrow(att_lm_data), ]


model_1 <- lm(est_ga ~ 0 + ., train)
pred_1 <- predict(model_1, test)

# Compute errors: error
error <- pred_1 - test$est_ga

# Calculate RMSE
sqrt(mean(error^2))

summary(model_1)
qqPlot(model_1, main = "QQ Plot")


### cross validation lm
model_2 <- train(
  est_ga ~ ., att_lm_data,
  method = "lm",
  tuneGrid  = expand.grid(intercept = FALSE),
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = TRUE
  )
)

model_2

summary(model_2)



### random forest model
model_3 <- train(
  est_ga ~.,
  tuneLength = 1,
  data = att_lm_data, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

model_3

### re-order main dataset


##generate predicted values on cross validation model
pred_cv <- data.frame(predict(model_2, att_lm_data))
colnames(pred_cv) <- c("pred_cv")

### fit model data back to og data set
att_model <- cbind(att_lm_data, pred_cv)
plot(att_model$pred_cv)


### generate random df
spend_sample <- c(300,400,500,600,700,800,900)
rand_spend <- data.frame(sample(spend_sample, 200, replace = T))
colnames(rand_spend ) <- c("SPEND")

dr_perc_sample <- c(0.70, 0.75, 0.80, 0.85, 0.90)
rand_dr <- data.frame(sample(dr_perc_sample, 200, replace = T))
colnames(rand_dr) <- c("DR_PERC")

rand_weekday <- c(0,1)
rand_wd <- data.frame(sample(rand_weekday, 200, replace = T))
colnames(rand_wd) <- c("WEEKDAY")

est_df <- cbind(rand_spend, rand_dr, rand_wd)

### fit model to random df
pred_random <- data.frame(predict(model_2, est_df))
colnames(pred_random) <- c("pred_rand")

att_model_rand <- cbind(est_df, pred_random)
plot(att_model_rand$pred_rand)



### estimate billing and net revenue
model_est <- att_model_rand %>%
  mutate(est_billing = pred_rand * 156) %>%
  mutate(est_net_rev = est_billing - SPEND)

model_est <- distinct(model_est)


model_wkend <- model_est %>%
  filter(WEEKDAY == 0)

model_wkday <- model_est %>%
  filter(WEEKDAY == 1)



plot(model_wkday$est_net_rev, model_wkday$SPEND) ## good
ggplot(model_wkday, aes(est_net_rev, SPEND)) + geom_bar()


plot(model_wkday$est_net_rev, model_wkday$SPEND) ## good

























### generate predicted values of random forest regression tree
pred_rf <- data.frame(predict(model_3, att_lm_data))
colnames(pred_cv) <- c("pred_rf")









att_mv$est_ga <- as.integer(att_mv$est_ga)
att_mv$DR_PERC <- round(att_mv$DR_PERC, 2)
att_mv$ID <- seq.int(nrow(att_mv))

att_mv_c <- att_mv %>% 
  select(-EVENT_DATE)


### model 1

lm_mv1 <- lm(est_ga ~ SPEND + DR_PERC, att_mv_c)

### test initial model
cor(att_mv_c, use = "complete.obs", method = "pearson")

outlierTest(lm_mv1)
qqPlot(lm_mv1, main = "QQ Plot")

plot(att_mv_c$DR_SPEND, att_mv_c$est_ga)
plot(att_mv_c$DR_PERC, att_mv_c$est_ga)


hist(att_mv_c$DR_SPEND)
hist(att_mv_c$UPPER_SPEND)
hist(att_mv_c$est_ga)
hist(att_mv_c$DR_PERC)


abline(lm_mv1$coefficients, col = "red")

summary(lm_mv1)



### model 2 clean


att_mv_C2 <- att_mv_c %>%
  filter(est_ga < 13) %>%
  filter(SPEND < 600) %>%
  filter(SPEND > 150) %>%
  filter(DR_SPEND >= 50) %>%
  filter(DR_PERC > 0.55)

hist(att_mv_C2$DR_SPEND)
hist(att_mv_C2$est_ga)
hist(att_mv_C2$DR_PERC)
hist(att_mv_C2$SPEND)

plot(att_mv_C2$DR_PERC, att_mv_C2$est_ga)
abline(lm_mv2$coefficients, col = "red")

### model 2

lm_mv2 <- lm(est_ga ~ SPEND + DR_PERC, att_mv_C2)

outlierTest(lm_mv2)
qqPlot(lm_mv2, main = "QQ Plot")

summary(lm_mv2)


spend_pred <- c(200,200,200,300,300,300,400,400,400,500,500,500,600,600,600,700,700,700,800,800,800,900,900,900)
dr_pred <- c(0.7, 0.8, 0.9)

new_data <- data.frame(SPEND = spend_pred, DR_PERC = dr_pred)


predict_mvlm <- predict.lm(lm_mv2, new_data, interval = "prediction", level = 0.9)

pred_df <- data.frame(new_data, predict_mvlm)


write.csv(pred_df, "predict_mvlm.csv")





