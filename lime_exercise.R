data("AdultUCI", package = "arules")
AdultUCI <- na.omit(AdultUCI)
AdultUCI$income <- factor(AdultUCI$income,
                          ordered = FALSE)
AdultUCI$education <- factor(AdultUCI$education,
                             ordered = FALSE)

set.seed(42)
AdultUCI <- AdultUCI[sample(1:nrow(AdultUCI), 1000),]

ind <- sample(1:nrow(AdultUCI), 5)
AdultUCI_explain <- AdultUCI[ind, -ncol(AdultUCI)]
AdultUCI_train <- AdultUCI[-ind, -ncol(AdultUCI)]
AdultUCI_lab <- AdultUCI[[ncol(AdultUCI)]][-ind]

model <- train(AdultUCI_train, AdultUCI_lab, method = "rf")

explainer <- lime(AdultUCI_train, model,
                  bin_continuous = TRUE,
                  n_bins = 4,
                  quantile_bins = TRUE)

# create explanations
explanation <- explain(AdultUCI_explain,
                       explainer,
                       n_labels = 1,
                       n_features = 3,
                       n_permutations = 5000,
                       feature_select = "auto")

plot_features(explanation)

