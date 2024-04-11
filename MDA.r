library(ggplot2) 
library(VIM)

# read the data set
# setwd("C:/Users/GOOD/Desktop")
data_assignment2 <- read.csv("datasetAssignment2.csv")

# data pre-processing
missing_data <- aggr(data_assignment2, plot = FALSE)
summary(missing_data)
cleaned_data <- na.omit(data_assignment2)
# head(cleaned_data)

##### q2 the important sociological variables
# linear regression model
model_1 <- lm(pvoixMACRON2022 ~ capitalimmo2022 + revmoyfoy2022 + revmoy2022 + ppropri2022 + 
              popagglo2022 + pop2021 + perbac2022 + persup2022 + pouvr2022 + 
              pcadr2022 + pempl2022 + pchom2022 + pagri2022, data = cleaned_data)

summary(model_1)
#par(mfrow=c(2,2))
#plot(model_1)

# find the best model
model_best <- step(model_1) 
summary(model_best)
plot(model_best)

##### q3 prediction
new_data_1 <- data.frame(
  popagglo2022 = 50000,  
  pcadr2022 = 0,  
  revmoyfoy2022 = 0,
  revmoy2022 = 0,
  ppropri2022 = 0,
  pop2021 = 0,
  perbac2022 = 0,
  persup2022 = 0,
  pchom2022 = 0,
  pagri2022 = 0
)
new_data_2 <- data.frame(
  popagglo2022 = 50000,  
  pcadr2022 = 0.25,  
  revmoyfoy2022 = 0,
  revmoy2022 = 0,
  ppropri2022 = 0,
  pop2021 = 0,
  perbac2022 = 0,
  persup2022 = 0,
  pchom2022 = 0,
  pagri2022 = 0
)
new_data_3 <- data.frame(
  popagglo2022 = 50000,  
  pcadr2022 = 0.25,  
  revmoyfoy2022 = 22000,
  revmoy2022 = 0,
  ppropri2022 = 0,
  pop2021 = 0,
  perbac2022 = 0,
  persup2022 = 0,
  pchom2022 = 0,
  pagri2022 = 0
)

Macron2022_1 <- predict(model_best, newdata = new_data_1)
Macron2022_2 <- predict(model_best, newdata = new_data_2)
Macron2022_3 <- predict(model_best, newdata = new_data_3)


cat("The predictive result 1 is", Macron2022_1, "\n")
cat("The predictive result 2 is", Macron2022_2, "\n")
cat("The predictive result 3 is", Macron2022_3, "\n")

Macron2022_inter_1 <- predict(model_best, newdata = new_data_1,
                              interval = "prediction", level = 0.95)
Macron2022_inter_2 <- predict(model_best, newdata = new_data_2,
                              interval = "prediction", level = 0.95)
Macron2022_inter_3 <- predict(model_best, newdata = new_data_3,
                              interval = "prediction", level = 0.95)

##### q5 PCA analysis
social_vars <- c("capitalimmo2022", "pcadr2022", "popagglo2022", 
                 "revmoyfoy2022", "revmoy2022", "ppropri2022", "pop2021", 
                 "perbac2022", "persup2022", "pagri2022", "pouvr2022",
                 "pempl2022", "pchom2022")

# pca
pca_data <- cleaned_data[, social_vars]
pca_result <- prcomp(pca_data, scale. = TRUE)
summary(pca_result)

# extract the rotation matrix
rotation_matrix <- pca_result$rotation
rotation_matrix

# extract the pca scores
pca_scores <- as.data.frame(predict(pca_result)[,1:7])

merged_data <- cbind(pca_scores, pvoixMACRON2022 = cleaned_data$pvoixMACRON2022)

# pca regression
model_pca <- lm(pvoixMACRON2022 ~ ., data = merged_data)
summary(model_pca)


##### q1
correlation_matrix <- cor(cleaned_data[, c("pvoixMACRON2022", "pvoixMACRON2017", 
                                           "pvoixMELENCHON2022", "pvoixMELENCHON2017", 
                                           "pvoixMLEPEN2022", "pvoixMLEPEN2017")])
correlation_matrix_all <- cor(cleaned_data[, c("pvoixMACRON2017", "pvoixMELENCHON2017", "pvoixMLEPEN2017", "pvoixHAMON2017", 
                                           "pvoixPOUTOU2017", "pvoixARTHAUD2017", "pvoixFILLON2017",
                                           "pvoixMACRON2022", "pvoixMELENCHON2022", "pvoixMLEPEN2022", "pvoixZEMMOUR2022", 
                                           "pvoixPOUTOU2022", "pvoixARTHAUD2022", "pvoixPECRESSE2022", "pvoixJADOT2022")])



ggplot(data = reshape2::melt(correlation_matrix)) +
  geom_tile(aes(Var1, Var2, fill = value)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap between Variables in 2017 and 2022")

print(correlation_matrix_all)
