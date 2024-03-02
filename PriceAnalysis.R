library(klaR)
library(caret)
library(rpart)
library(rpart.plot)
a_vehicles <- read.csv('Australian Vehicle Prices.csv')
colnames(a_vehicles)
a_veh_simp <- subset(a_vehicles,Price != c("POA", ""), select =  c(1,2,6,19))
selected_brands <- c("Toyota", "Mitsubishi", "Mazda", "Kia")
a_veh_simp <- a_veh_simp[a_veh_simp$Brand %in% selected_brands, ]
a_veh_simp$Price <- as.numeric(as.character(a_veh_simp$Price))
summary(a_veh_simp$Price)
a_veh_simp$p_sections <- cut(a_veh_simp$Price, breaks = c(0,19990,29900,41990,299901), labels = c("Lowest Prices", "Lower Prices", "Higher Prices", "Highest Prices"))
set.seed(123)
index_split <- createDataPartition(a_veh_simp$p_sections, p = 0.8, list = FALSE)
veh_train <- a_veh_simp[index_split, ]
veh_test <- a_veh_simp[-index_split, ]
table(a_veh_simp$Year, a_veh_simp$p_sections)
table(a_veh_simp$UsedOrNew, a_veh_simp$p_sections)
table(a_veh_simp$Brand, a_veh_simp$p_sections)
tree <- rpart(p_sections ~ UsedOrNew + Year + Brand, data = veh_train, method = "class")
tree
rpart.plot(tree)
rpart_predictions <- predict(tree, veh_test, type = "class")
length(rpart_predictions)
length(a_veh_simp$p_sections)
rpart_confusion_matrix <- confusionMatrix(rpart_predictions, veh_test$p_sections)
rpart_confusion_matrix
naive_bayes <- NaiveBayes(p_sections ~ UsedOrNew + Year + Brand, data = veh_train)
naive_predictions <- predict(naive_bayes, veh_test)
naive_confusion_matrix <- confusionMatrix(naive_predictions$class, veh_test$p_sections)
naive_confusion_matrix
