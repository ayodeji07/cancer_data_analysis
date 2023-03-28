
# importing data


library(readr)
cancer_data <- read_csv("cancer data for MOOC 1.csv")
View(cancer_data)


# Output the 4th row and all colums
cancer_data[4,]


# output yhe 5th column and all rows
cancer_data[,5]


# Output the data in the 4th row and 5th column
cancer_data[4,5]


# Output all columns in the first five rows
cancer_data[1:5,]


# Output all rows of the first five columns
cancer_data[,1:5]


# Dimension of the data
dim(cancer_data)

#gender <- as.factor(cancer_data[,'gender'])

# Count of individual categories in gender column
gender <- cancer_data[,'gender']
table(gender)

# Summary of bmi column
bmi <- cancer_data[,'bmi']
summary(bmi)


#calculating a new column
fruit <- cancer_data[,'fruit']
veg <- cancer_data[,'veg']
fruitveg <- fruit + veg
table(fruitveg)

# Adding the column to the data
cancer_data$fruitveg <- fruitveg

typeof(cancer_data$fruitveg)

# Changed the datatype of fruitveg from list to numeric
cancer_data$fruitveg <- as.numeric(unlist(cancer_data$fruitveg))
typeof(cancer_data$fruitveg)


hist(cancer_data$fruitveg)


hist(cancer_data$age)

cancer_data$smoking

table(cancer_data$smoking, exclude = NULL)

summary(fruitveg)


# Seperating column int groups

cancer_data$five_a_day <- ifelse(cancer_data$fruitveg >= 5, 1, 0)

table(cancer_data$five_a_day)

hist(cancer_data$fruitveg, xlab = "Portions of fruits and vegetables",
     main = "Daily consumption of fruit and vegetable combined", axes =  F)
axis(side = 1, at = seq(0, 11, 1))
axis(side = 2, at = seq(0, 16, 2))

library(ggplot2)
ggplot() + geom_histogram(data = cancer_data, aes(x = cancer_data$fruitveg),
                          bins = 10, fill = "darkgreen", col= "black") +
  labs(x = "Portions of fruits and vegetables", y ="frequency") +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_bw()



# Seperating bmi into "normal" and "not normal"
cancer_data$healthy_bmi <- ifelse(cancer_data$bmi >= 18.5 & cancer_data$bmi <= 24.9, 1, 0)
table(cancer_data$healthy_bmi)

ggplot() + geom_histogram(data = cancer_data, aes(x = cancer_data$healthy_bmi),
                          bins = 10, fill = "darkgreen", col= "black") +
  labs(x = "BMI Distributions", y ="frequency") +
  scale_x_continuous(breaks = seq(from = 0, to = 1)) + theme_bw()
                     
# chi-squared test
# proportion of people getting their five a dy versus proportion of people who had cancer
chisq.test(x=cancer_data$five_a_day, y=cancer_data$cancer)

# t-test
#predicting bmi using cancer
t.test(cancer_data$bmi~cancer_data$cancer)

t.test(cancer_data$bmi~cancer_data$cancer,var.equal = TRUE)

# Separating bmi into 'overweight' and 'not overweight'
cancer_data$overweight = ifelse(cancer_data$bmi > 25, 1, 0)

table(cancer_data$overweight)

#Performing chi-square on overweight and cancer
chisq.test(x = cancer_data$overweight, y = cancer_data$cancer)

