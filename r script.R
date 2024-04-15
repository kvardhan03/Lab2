
# Loading the student data using base R read.csv function
students <- read.csv("C:/Users/kvard/Downloads/oulad-students.csv")

#Display the dataset.
students

# Data preprocessing: Transform 'final_result' into a binary variable and 'disability' into a factor
students$is_passed <- as.factor(ifelse(students$final_result == "Pass", 1, 0))
students$credits <- as.factor(students$studied_credits)


# Convert 'imd_band' to a numeric scale based on given categories
imd_scale <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
students$imd_numeric <- as.numeric(factor(students$imd_band, levels = imd_scale))

# Creating train and test sets manually
set.seed(20230712)  # Setting seed for reproducibility
sample_count <- floor(0.8 * nrow(students)) #Splitting the data into 80% as train data and 20% as test data.
training <- sample(seq_len(nrow(students)), size = sample_count)

train_data <- students[training, ]
test_data <- students[-training, ]

# Building a logistic regression model with glm (Generalized Linear Model) in base R
logit_model <- glm(is_passed ~ credits + imd_numeric, family = binomial(link = "logit"), data = train_data)

# Model summary display.
summary(logit_model)

#Tidying the model 
library(tidymodels)
tidy(logit_model)

#Plotting the model.
plot(logit_model)
