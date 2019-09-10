library(DALEX)
data("dragons")
data("dragons_test")

head(dragons)

set.seed(123)

dragon_rf_model <- 
  randomForest::randomForest(life_length ~ ., 
                             data = dragons)

dragon_lm_model <- lm(life_length ~ ., 
                      data = dragons)


predicted_rf <- predict(dragon_rf_model, 
                        dragons_test)

predicted_lm <- predict(dragon_lm_model,
                        dragons_test)


# RMSE

sqrt(mean((dragons_test$life_length - predicted_rf)^2))
sqrt(mean((dragons_test$life_length - predicted_lm)^2))

# explainer

explainer_lm <- DALEX::explain(model = dragon_lm_model, 
                               data = dragons_test[,-ncol(dragons_test)], 
                               y = dragons_test$life_length)
explainer_rf <- DALEX::explain(model = dragon_rf_model, 
                               data = dragons_test[,-ncol(dragons_test)], 
                               y = dragons_test$life_length)

mp_lm <- model_performance(explainer_lm)
mp_rf <- model_performance(explainer_rf)

plot(mp_lm, mp_rf, geom = "boxplot")

vi_rf <- ingredients::feature_importance(
  explainer_rf,
  loss_function = loss_root_mean_square)

vi_lm <- ingredients::feature_importance(
  explainer_lm,
  loss_function = loss_root_mean_square)

vi_rf

plot(vi_rf, vi_lm)



ggplot(mp_rf, 
       aes(observed, diff)) +
  geom_point() + 
  xlab("Observed") + 
  ylab("Predicted - Observed") + 
  ggtitle("Diagnostic plot") + 
  theme_mi2()

ggplot(mp_lm, 
       aes(observed, diff)) +
  geom_point() + 
  xlab("Observed") + 
  ylab("Predicted - Observed") + 
  ggtitle("Diagnostic plot") + 
  theme_mi2()
