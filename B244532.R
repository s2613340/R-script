#install packages
install.packages("readxl")
install.packages("AER")
install.packages("ggplot2")
install.packages("gridExtra")
#get data from file
#change the path where the data is stored
library(readxl)
FILE <- read_excel("biomarkers_covariates.xlsx", sheet = "data")

#declare the variables from data file
NUM <- FILE$'NUM'
GENDER <- FILE$'Sex (1=male, 2=female)'
VAS_INCLUSION <- FILE$'VAS-at-inclusion'
VAS_12MONTHS <- FILE$'Vas-12months'
IL_8 <- FILE$'IL-8'
VEGF_A <- FILE$'VEGF-A'
OPG <- FILE$'OPG'
TGF_BETA_1 <- FILE$'TGF-beta-1'
IL_6 <- FILE$'IL-6'
CXCL9 <- FILE$'CXCL9'
CXCL1 <- FILE$'CXCL1'
IL_18 <- FILE$'IL-18'
CSF_1 <- FILE$'CSF-1'
BIOMARKER<- FILE$'Biomarker'
AGE <- FILE$'Age'
SMOKER <- FILE$'Smoker (1=yes, 2=no)'

#Question1
#create data frame
df <- data.frame(NUM,GENDER,VAS_INCLUSION,VAS_12MONTHS,IL_8,VEGF_A,OPG,TGF_BETA_1,IL_6,CXCL9,CXCL1,IL_18
                 ,CSF_1,BIOMARKER,AGE,SMOKER)
df$GENDER <- factor(df$GENDER, levels = c("1", "2"), labels = c("Male", "Female"))
df$SMOKER <- factor(df$SMOKER, levels = c("1", "2"), labels = c("Smoker", "Non-smoker"))
library(ggplot2)
library(gridExtra)

# Make sure your 'GENDER' column is a factor with levels properly labeled as "Male" and "Female"
# Filter the dataframe for the condition "0weeks"
df_0weeks <- df[df$BIOMARKER == "0weeks", ]

#Check normal for IL_8
plot_IL_8_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = IL_8)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of IL_8 Levels for Females at 0 weeks") +
  theme_bw()

plot_IL_8_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = IL_8)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of IL_8 Levels for Males at 0 weeks") +
  theme_bw()

#Check normal for VEGF_A
plot_VEGF_A_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = VEGF_A)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of VEGF_A Levels for Females at 0 weeks") +
  theme_bw()

plot_VEGF_A_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = VEGF_A)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of VEGF_A Levels for Males at 0 weeks") +
  theme_bw()

#Check normal for OPG
plot_OPG_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = OPG)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of OPG Levels for Females at 0 weeks") +
  theme_bw()

plot_OPG_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = OPG)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of OPG Levels for Males at 0 weeks") +
  theme_bw()

#Check normal for TGF_BETA_1
plot_TGF_BETA_1_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = TGF_BETA_1)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of TGF_BETA_1 Levels for Females at 0 weeks") +
  theme_bw()

plot_TGF_BETA_1_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = TGF_BETA_1)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of TGF_BETA_1 Levels for Males at 0 weeks") +
  theme_bw()

#Check normal for IL_6
plot_IL_6_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = IL_6)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of IL_6 Levels for Females at 0 weeks") +
  theme_bw()

plot_IL_6_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = IL_6)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of IL_6 Levels for Males at 0 weeks") +
  theme_bw()

#Check normal for CXCL9
plot_CXCL9_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = CXCL9)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of CXCL9 Levels for Females at 0 weeks") +
  theme_bw()

plot_CXCL9_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = CXCL9)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of CXCL9 Levels for Males at 0 weeks") +
  theme_bw()

#Check normal for CXCL1
plot_CXCL1_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = CXCL1)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of CXCL1 Levels for Females at 0 weeks") +
  theme_bw()

plot_CXCL1_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = CXCL1)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of CXCL1 Levels for Males at 0 weeks") +
  theme_bw()


#Check normal for IL_18
plot_IL_18_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = IL_18)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of IL_18 Levels for Females at 0 weeks") +
  theme_bw()

plot_IL_18_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = IL_18)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of IL_18 Levels for Males at 0 weeks") +
  theme_bw()

#Check normal for CSF_1
plot_CSF_1_female<- ggplot(df[df$GENDER == "Female" & df$BIOMARKER == "0weeks", ], aes(sample = CSF_1)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of CSF_1 Levels for Females at 0 weeks") +
  theme_bw()

plot_CSF_1_male<- ggplot(df[df$GENDER == "Male" & df$BIOMARKER == "0weeks", ], aes(sample = CSF_1)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ plot of CSF_1 Levels for Males at 0 weeks") +
  theme_bw()

# Arrange the plots in a grid
grid.arrange(
  plot_IL_8_female, plot_IL_8_male, 
  ncol = 2 # Or another number that fits your layout needs
)

grid.arrange(
  plot_VEGF_A_female, plot_VEGF_A_male, 
  ncol = 2 # Or another number that fits your layout needs
)

grid.arrange(
  plot_OPG_female, plot_OPG_male, 
  ncol = 2 # Or another number that fits your layout needs
)


grid.arrange(
  plot_TGF_BETA_1_female, plot_TGF_BETA_1_male, 
  ncol = 2 # Or another number that fits your layout needs
)

grid.arrange(
  plot_IL_6_female, plot_IL_6_male, 
  ncol = 2 # Or another number that fits your layout needs
)

grid.arrange(
  plot_CXCL9_female, plot_CXCL9_male, 
  ncol = 2 # Or another number that fits your layout needs
)

grid.arrange(
  plot_CXCL1_female, plot_CXCL1_male, 
  ncol = 2 # Or another number that fits your layout needs
)

grid.arrange(
  plot_IL_18_female, plot_IL_18_male, 
  ncol = 2 # Or another number that fits your layout needs
)

grid.arrange(
  plot_CSF_1_female, plot_CSF_1_male, 
  ncol = 2 # Or another number that fits your layout needs
)
#summary
summary_IL_8 <- aggregate(IL_8 ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_IL_8

summary_VEGF_A <- aggregate(VEGF_A ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_VEGF_A

summary_OPG<- aggregate(OPG ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_OPG

summary_TGF_BETA_1 <- aggregate(TGF_BETA_1 ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_TGF_BETA_1

summary_IL_6 <- aggregate(IL_6 ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_IL_6

summary_CXCL9 <- aggregate(CXCL9 ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_CXCL9

summary_CXCL1 <- aggregate(CXCL1 ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_CXCL1

summary_IL_18 <- aggregate(IL_18 ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_IL_18

summary_CSF_1 <- aggregate(CSF_1 ~ GENDER, data = df[df$BIOMARKER == "0weeks", ], FUN = function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    count = length(x))
})
summary_CSF_1



#perform t test
T_TEST_IL8        <- t.test(IL_8 ~ GENDER, data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_VEGF_A     <-t.test(VEGF_A~GENDER, data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_OPG        <-t.test(OPG~GENDER,data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_TGF_BETA_1 <-t.test(TGF_BETA_1~GENDER,data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_IL_6       <-t.test(IL_6~GENDER,data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_CXCL9      <-t.test(CXCL9~GENDER,data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_CXCL1      <-t.test(CXCL1~GENDER,data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_IL_18      <-t.test(IL_18~GENDER,data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)
T_TEST_CSF_1      <-t.test(CSF_1~GENDER,data = df, subset = (BIOMARKER == "0weeks"), mu = 0, alternative = "two.sided", conf.level = 0.95)

T_TEST_IL8        
T_TEST_VEGF_A     
T_TEST_OPG        
T_TEST_TGF_BETA_1 
T_TEST_IL_6        
T_TEST_CXCL9      
T_TEST_CXCL1      
T_TEST_IL_18      
T_TEST_CSF_1      

# Bonferroni correction
corrected_alpha <- 0.05 / 9

# Check if the p-value is less than the corrected alpha

BONFERRONI_IL8 <- if(T_TEST_IL8$p.value < corrected_alpha) {
  print("The testing result for IL8 is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for IL8 is not statistically significant after Bonferroni correction.")
}

BONFERRONI_VEGF_A <- if(T_TEST_VEGF_A$p.value < corrected_alpha) {
  print("The testing result for VEGF_A is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for VEGF_A is not statistically significant after Bonferroni correction.")
}

BONFERRONI_TGF_BETA_1 <- if(T_TEST_TGF_BETA_1$p.value < corrected_alpha) {
  print("The testing result for TGF_BETA_1 is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for TGF_BETA_1 is not statistically significant after Bonferroni correction.")
}

BONFERRONI_IL_6 <- if(T_TEST_IL_6$p.value < corrected_alpha) {
  print("The testing result for IL_6 is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for IL_6 is not statistically significant after Bonferroni correction.")
}

BONFERRONI_CXCL9 <- if(T_TEST_CXCL9$p.value < corrected_alpha) {
  print("The testing result for CXCL9 is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for CXCL9 is not statistically significant after Bonferroni correction.")
}

BONFERRONI_CXCL1 <- if(T_TEST_CXCL1$p.value < corrected_alpha) {
  print("The testing result for CXCL1 is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for CXCL1 is not statistically significant after Bonferroni correction.")
}

BONFERRONI_IL_18 <- if(T_TEST_IL_18$p.value < corrected_alpha) {
  print("The testing result for IL_18 is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for IL_18 is not statistically significant after Bonferroni correction.")
}

BONFERRONI_CSF_1 <- if(T_TEST_CSF_1$p.value < corrected_alpha) {
  print("The testing result for CSF_1 is statistically significant after Bonferroni correction.")
} else {
  print("The testing result for CSF_1 is not statistically significant after Bonferroni correction.")
}


#Question2

# Load the necessary libraries which use for machine learning in R
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

# Filter the dataframe for the biomarker at '0weeks'
filtered_df <- df[df$BIOMARKER == '0weeks', ] 

# Select the relevant columns 
cor_df <- filtered_df[, c("VAS_12MONTHS","IL_8","VEGF_A","OPG","TGF_BETA_1","IL_6","CXCL9","CXCL1","IL_18","CSF_1","AGE")]
correlation_matrix <- cor(cor_df, use="complete.obs")  # there might be NA values, use complete.obs to handle 
print(correlation_matrix)

# Create the regression dataset 
regression_data <- filtered_df[, c("NUM","VAS_12MONTHS","IL_8","VEGF_A","OPG","TGF_BETA_1","IL_6","CXCL9","CXCL1","IL_18","CSF_1","AGE","GENDER","SMOKER")]
regression_data_use <- as.data.frame(regression_data)

# set the seed for random number generation
set.seed(123)

# Create stratified split indices
splitIndex <- createDataPartition(regression_data_use$GENDER, p = 0.8, list = FALSE)

# Split the data into training and test sets
train_data <- regression_data_use[splitIndex, ]
test_data <- regression_data_use[-splitIndex, ]

# Fit the linear model on the training set
model <- lm(VAS_12MONTHS ~ IL_8 + VEGF_A + OPG + TGF_BETA_1 + IL_6 + CXCL9 + CXCL1 + IL_18 + CSF_1 + AGE + SMOKER + GENDER, data = train_data)
model

# Get the summary of the model
summary_model <- summary(model)

# Print the summary
print(summary_model)

# R-squared and Adjusted R-squared
cat("R-squared:", summary_model$r.squared, "\n")
cat("Adjusted R-squared:", summary_model$adj.r.squared, "\n")

# Residual Standard Error
cat("Residual Standard Error:", summary_model$sigma, "\n")

# Coefficients p-values
print(coef(summary(model)))


predicted_vas <- predict(model, newdata = test_data)

# Compare predicted to actual VAS scores
comparison <- data.frame(Actual = test_data$VAS_12MONTHS, Predicted = predicted_vas)
print(comparison)


# Calculate R-squared for the test data
SStot_test <- sum((test_data$VAS_12MONTHS - mean(train_data$VAS_12MONTHS))^2)
SSres_test <- sum((predicted_vas - test_data$VAS_12MONTHS)^2)
rsquared_test <- 1 - (SSres_test / SStot_test)
cat("R-squared for test data:", rsquared_test, "\n")


