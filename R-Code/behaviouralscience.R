library(readxl)
Drug_Abuse <- read_excel("C:/Users/Shruti George/Downloads/R Programming/CAC II-A/Drug_Abuse.xlsx")
View(Drug_Abuse)

#Defining Consumption Levels for the parameters
Drug_Abuse$Alcohol = ordered(Drug_Abuse$Alcohol, levels = c("CL6", "CL5", "CL4", "CL3", "CL2", "CL1", "CL0"))
Drug_Abuse$Cannabis = ordered(Drug_Abuse$Cannabis, levels = c("CL6", "CL5", "CL4", "CL3", "CL2", "CL1", "CL0"))
Drug_Abuse$Nicotine = ordered(Drug_Abuse$Nicotine, levels = c("CL6", "CL5", "CL4", "CL3", "CL2", "CL1", "CL0"))

#Question 1:
#Conduct univariate analysis on both numerical and categorical variables in the dataset.

#Univariate Analysis on Categorical Data
#A) Number of People Belonging to different Age Groups
table(Drug_Abuse$Age)

#B) Number of People Belonging to different Qualification
table(Drug_Abuse$Education)

#C) Bar Plot to Visualise Different Countries
country_count = table(Drug_Abuse$Country)
country_count
barplot(country_count, main = "Bar Chart of country",
        xlab = "Countries", ylab = "Frequency", col = "skyblue")

#D) Pie Chart to Represent Weightage of Gender
gender_count = table(Drug_Abuse$Gender)
gender_count
labels=paste(names(gender_count), "(", gender_count, ")", sep = " ")
colors = c("lightpink","darkblue")
pie(gender_count, main = "Pie Chart of Drug Abusers (by Gender)",
    col = colors)
legend("topright", legend = labels, cex = 0.8, fill = colors, bty = "n")

#Univariate Analysis on Numerical Data
#A) Mean and Variance of Neuroticism Score (NScore)
mean(Drug_Abuse$Nscore)
var(Drug_Abuse$Nscore)

#B) Displaying Minimum and Maximum Value of Extraversion Score (EScore)
min(Drug_Abuse$Escore)
max(Drug_Abuse$Escore)

#C) Boxplot of Agreeableness Score (AScore)
boxplot(Drug_Abuse$AScore, main = "Boxplot of Agreeableness Score", col = "lightblue")

#D)Plot Density Chart of Conscientiousness Score (CScore)
plot(density(Drug_Abuse$Cscore),main = "Density Plot of CScore",col="magenta")

#E) Summary Table for Openness to experience Score (OScore)
summary(Drug_Abuse$Oscore)


#Question 2:
#Demonstrate the conversion of numerical to categorical. 
#Rename the categorical values with appropriate codes. 
#Demonstrate the basic descriptive statistics for each of the 
#categorical value as obtained.

#A) Converting Neuroticism Score to Scale
cuts = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)
Drug_Abuse$NScale = ordered(cut(Drug_Abuse$Nscore, breaks = cuts, labels = c("Very Low", "Low", "Medium", "High", "Very High")))
head(Drug_Abuse$NScale)

#B) Converting Extraversion Score to Scale
Drug_Abuse$EScale = ordered(cut(Drug_Abuse$Escore, breaks = cuts, labels = c("Very Low", "Low", "Medium", "High", "Very High")))
head(Drug_Abuse$EScale)

#C) Converting Agreeableness Score to Scale
Drug_Abuse$AScale = ordered(cut(Drug_Abuse$AScore, breaks = cuts, labels = c("Very Low", "Low", "Medium", "High", "Very High")))
head(Drug_Abuse$AScale)

#D) Converting Openness Score to Scale
Drug_Abuse$OScale = ordered(cut(Drug_Abuse$Oscore, breaks = cuts, labels = c("Very Low", "Low", "Medium", "High", "Very High")))
head(Drug_Abuse$OScale)

#E) Converting Conscientiousness Score to Scale
Drug_Abuse$CScale = ordered(cut(Drug_Abuse$Cscore, breaks = cuts, labels = c("Very Low", "Low", "Medium", "High", "Very High")))
head(Drug_Abuse$CScale)

#F) Performing descriptive statistics on NScale Variable
# Assuming 'NScale' is a categorical variable in your dataset
NScale_counts=table(Drug_Abuse$NScale)
NScale_percentage=prop.table(NScale_counts) * 100
NScale_percentage
NScale_summary=data.frame(NScale = names(NScale_counts),
                             Count = as.numeric(NScale_counts),
                             Percentage = NScale_percentage)
print(NScale_summary)

# Frequency distribution
print("Frequency Distribution of NScale:")
print(NScale_counts)

# Display percentage distribution
print("Percentage Distribution of NScale:")
print(NScale_percentage)

# Calculating Mode of Distribution
NScale_counts <- table(Drug_Abuse$NScale)
NScale_mode <- names(NScale_counts)[NScale_counts == max(NScale_counts)]
print("Mode of NScale:")
print(NScale_mode)

# Calculating the Range of Distribution
NScale_range <- diff(range(NScale_counts))
print("Range of NScale:")
print(NScale_range)

# Calculate the Interquartile Range (IQR) of Distribution
NScale_quartiles <- quantile(NScale_counts, probs = c(0.25, 0.75))
NScale_IQR <- diff(NScale_quartiles)
print("Interquartile Range (IQR) of NScale:")
print(NScale_IQR)

# Bar plot of NScale
barplot(NScale_counts, main = "Frequency Distribution of NScale", xlab = "NScale Categories", ylab = "Frequency", col="darkgreen")

# Pie chart of NScale
pie(NScale_counts, main = "Pie Chart of NScale")


#Question 3:
#Conduct bivariate analysis on the following categories of variables in the dataset:
#a. Numerical vs Numerical
#b. Categorical vs Categorical
#c. Categorical vs Numerical.

#Numerical vs Numerical
# A) Scatter plot between OScore and Nscore
plot(Drug_Abuse$Oscore, Drug_Abuse$Nscore, xlab = "Oscore", ylab = "Nscore", main = "Scatter plot of Age vs Nscore", col="darkred")

# B) Scatter plot between Escore and Ascore
plot(Drug_Abuse$Escore, Drug_Abuse$AScore, xlab = "Escore", ylab = "Ascore", main = "Scatter plot of Escore vs Ascore", col="blue")

# C) Correlation analysis of the 5 Personality Traits
correlation_matrix = cor(Drug_Abuse[, c("Nscore", "Escore", "Oscore","AScore", "Cscore")])
print(correlation_matrix)

#Categorical Vs Categorical
#A) Contingency table of Gender and Alcohol Consumption
contingency_table1 <- table(Drug_Abuse$Gender, Drug_Abuse$Alcohol)
print(contingency_table1)

#B) Contingency table of Country and Nicotine Abuse
contingency_table2 <- table(Drug_Abuse$Country, Drug_Abuse$Nicotine)
print(contingency_table2)

#C) Contingency Table of Educational Qualification and Cannabis Consumption
contingency_table3 <- table(Drug_Abuse$Education, Drug_Abuse$Cannabis)
print(contingency_table3)

#D) Contingency table of Educational Qualification and Alcohol Consumption
contingency_table4 <- table(Drug_Abuse$Education,Drug_Abuse$Alcohol)
print(contingency_table4)

#E) Contingency table of Country and Cannabis Consumption
contingency_table5 <- table(Drug_Abuse$Country, Drug_Abuse$Cannabis)
contingency_table5


#Numerical Vs Categorical
# A) Boxplot of Nscore vs Alcohol Consumption
boxplot(Nscore ~ Alcohol, data = Drug_Abuse, 
        main = "Boxplot of Nscore by Alcohol Consumption",
        col = c("lightblue","lightgreen","lightpink"),
        xlab = "Alcohol Consumption", ylab = "Nscore")

# B) Boxplot of Ascore vs Nicotine Abuse
boxplot(AScore ~ Nicotine, data = Drug_Abuse, 
        main = "Boxplot of Ascore by Nicotine Consumption",
        col = c("lightgrey", "lavender","lightblue","skyblue","blue","darkblue","purple"),
        xlab = "Nicotine Consumption", ylab = "Ascore")

# C) Perform bivariate analysis using aggregate (standard deviation)
bivariate_result <- aggregate(Escore ~ Cannabis, data = Drug_Abuse, FUN = sd)
print(bivariate_result)

# D) Perform bivariate analysis using aggregate (mean)
bivariate_result2 <- aggregate(Cscore ~ Cannabis, data = Drug_Abuse, FUN = mean)
print(bivariate_result2)

