## Step 1: Loading and Preparing the Data
# Load necessary libraries
library(tibble)
library(caret)
library(dplyr)
library(readr)

# Impoprting Data Set
pitches <- read_csv(
  "data/pacheco_pitch_data.csv",
  col_types = cols(
    TaggedPitchType = col_factor(
      levels = c("Four-Seam", "Sinker", "Changeup", "Slider")
    )
  )
)

# Turning "Pitch Call" into a logistical expression
pacheco <- Pacheco %>%
  filter(PitchCall %in% c("BallCalled","StrikeCalled","StrikeSwinging")) %>%
  mutate(PitchCall = ifelse(PitchCall %in% c("StrikeCalled","StrikeSwinging"), 1, 0))
 # Inspec the generated data
head(pacheco)

## Step 2: Splitting the Data into Training and Testing Sets
# Split data into training (70%) and testing (30%)
set.seed(123)  # For reproducibility
PachecotrainIndex <- createDataPartition(pacheco$PitchCall, p = 0.7, list = FALSE)
PachecotrainData <- pacheco[PachecotrainIndex,]
PachecotestData <- pacheco[-PachecotrainIndex,]

## Step 3: Fitting the Logistic Regression Model
# Fit a logistic regression model using glm()
Pacheco_glm <- glm(PitchCall ~ TaggedPitchType + Extension + Balls + Strikes, data = PachecotrainData, family = binomial())

# Summary of the model
summary(Pacheco_glm)
coef(Pacheco_glm)
ORs = exp(Pacheco_glm$coefficients)
CIs = exp(confint(Pacheco_glm))
cbind(ORs, CIs)

## Step 4: Making Predictions
# Predicting probabilities on the test set
Pacheco_predictions <- predict(Pacheco_glm, newdata = PachecotestData, type = "response")

# Convert probabilities to binary outcomes (threshold of 0.5)
Pacheco_predicted_classes <- ifelse(Pacheco_predictions > 0.5, 1, 0)


## Step 5: Evaluating the Model Performance
# Confusion matrix to evaluate accuracy
confusionMatrix <- confusionMatrix(factor(Pacheco_predicted_classes), factor(PachecotestData$PitchCall))
confusionMatrix

# 1. Accuracy of the model
accuracy <- confusionMatrix$overall["Accuracy"]
accuracy

# 2. ROC Curve and AUC
library(pROC)
roc_curve <- roc(PachecotestData$PitchCall, Pacheco_predictions)
plot(roc_curve)
auc(roc_curve)
