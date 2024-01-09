library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(randomForest)

list2env(tt_load('2023-08-15'), globalenv())

# convert target variable to factor
spam <- spam %>% mutate(yesno = factor(yesno == 'y', levels=c(TRUE, FALSE)))

# split into training and test sets
set.seed(888)
spl <- caTools::sample.split(spam$yesno, 0.7)
train <- subset(spam, spl == TRUE)
test <- subset(spam, spl == FALSE)

# random forest model build and evaluation of class prediction accuracy using confusion matrix
rf <- randomForest(yesno ~ ., data = train, importance = TRUE) 
clf.predict <- predict(rf, newdata = test)

# accuracy of nearly 88%, avoids false negatives 94% of the time
caret::confusionMatrix(data = clf.predict, test$yesno)


# extract importance measure: gini index node impurity
# measures the mean decrease in node impurity after a split in the decision trees
# the higher the mean decrease, the higher the degree to which the variable makes the node homogeneous
imp_df <- as.data.frame(importance(rf))

rownames(imp_df) <- c("total length uninterrupted caps", "% is dollar sign ", "% is !",
             "% is word money", "% is 000 string", 
             "% is word make")

imp_df <- tibble::rownames_to_column(imp_df, "var")
imp_df$var<- imp_df$var %>% as.factor()

# plot feature importance
values <- c("#d00060","#f84239", "#9ba9c2", "#2b416c", "#046b99", "#102349")
  
p <- ggplot(data = imp_df) + 
  geom_bar(stat = "identity", mapping = aes(x = var, y = MeanDecreaseGini, fill = var), 
    show.legend = FALSE, width = 1) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  labs(x = NULL, y = "gini index node impurity", title = "random forest feature importance: which email elements matter most for detecting spam",
       subtitle = "the degree to which the variable makes a decision tree node homogeneous") +
  coord_polar() + 
  theme_minimal() +
  scale_fill_manual(values = values)

p

# varImpPlot(rf) # not run

