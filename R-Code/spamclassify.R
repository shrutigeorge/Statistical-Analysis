library("readxl")
data = read_excel("C:/Users/shrut/Downloads/spam_detection_dataset.xlsx")
View(data)

data$is_spam = as.factor(data$is_spam)
data$has_offer = as.factor(data$has_offer)
data$all_caps = as.factor(data$all_caps)

head(data)
str(data)
summary(data)

# Overall spam rate
spam_rate = mean(as.numeric(as.character(data$is_spam)))
total_emails = nrow(data)
spam_count = sum(data$is_spam == 1)
ham_count = total_emails - spam_count

cat("Dataset Overview:\n")
cat("Total emails:", total_emails, "\n")
cat("Spam emails:", spam_count, "\n")
cat("Ham emails:", ham_count, "\n")
cat("Overall spam rate:", round(spam_rate, 4), "\n")

#Binomial Tests
# Test if spam rate = 50%
binom_test_50 = binom.test(spam_count, total_emails, p = 0.5)
print("Binomial test - Is spam rate = 50%?")
print(binom_test_50)

# Confidence interval for spam rate
spam_ci = binom.test(spam_count, total_emails)
print("Confidence interval for spam rate:")
print(spam_ci)


# Categorize features
data$links_category = ifelse(data$num_links == 0, "No Links", 
                            ifelse(data$num_links <= 2, "Few Links", "Many Links"))

data$words_category = cut(data$num_words, 
                         breaks = quantile(data$num_words, c(0, 0.33, 0.66, 1), na.rm = TRUE), 
                         labels = c("Short", "Medium", "Long"), 
                         include.lowest = TRUE)

data$sender_score_category = cut(data$sender_score, 
                                breaks = c(0, 0.6, 0.8, 1), 
                                labels = c("Low", "Medium", "High"), 
                                include.lowest = TRUE)

# Analysis by Links Category
cat("\nSPAM RATE BY LINKS CATEGORY \n")
links_table = table(data$links_category, data$is_spam)
print(links_table)
print(prop.table(links_table, 1))

for(category in unique(data$links_category)) {
  subset_data = data[data$links_category == category, ]
  spam_in_category = sum(subset_data$is_spam == 1)
  total_in_category = nrow(subset_data)
  
  cat("\nBinomial test for", category, ":\n")
  test_result = binom.test(spam_in_category, total_in_category)
  cat("Spam rate:", round(spam_in_category / total_in_category, 4), "\n")
  cat("95% CI: [", round(test_result$conf.int[1], 4), ",", 
      round(test_result$conf.int[2], 4), "]\n")
}

# Analysis by Word Count Category
cat("\nSPAM RATE BY WORD COUNT CATEGORY\n")
words_table = table(data$words_category, data$is_spam)
print(words_table)
print(prop.table(words_table, 1))

# Analysis by Has Offer
cat("\nSPAM RATE BY HAS OFFER\n")
offer_table = table(data$has_offer, data$is_spam)
print(offer_table)
print(prop.table(offer_table, 1))

offer_spam = sum(data$is_spam[data$has_offer == 1] == 1)
offer_total = sum(data$has_offer == 1)
no_offer_spam = sum(data$is_spam[data$has_offer == 0] == 1)
no_offer_total = sum(data$has_offer == 0)

cat("Emails with offers - Spam rate:", round(offer_spam / offer_total, 4), "\n")
cat("Emails without offers - Spam rate:", round(no_offer_spam / no_offer_total, 4), "\n")

prop_test_offer = prop.test(c(offer_spam, no_offer_spam), c(offer_total, no_offer_total))
cat("\nTwo-proportion test (offer vs no offer):\n")
print(prop_test_offer)

# Analysis by All Caps
cat("\nSPAM RATE BY ALL CAPS\n")
caps_table = table(data$all_caps, data$is_spam)
print(caps_table)
print(prop.table(caps_table, 1))

#Chi-Square Test for Independence
cat("\nCHI-SQUARE TESTS\n")

chisq_links = chisq.test(data$links_category, data$is_spam)
print("Chi-square test - Links category vs Spam:")
print(chisq_links)

chisq_offer = chisq.test(data$has_offer, data$is_spam)
print("Chi-square test - Has offer vs Spam:")
print(chisq_offer)

chisq_caps = chisq.test(data$all_caps, data$is_spam)
print("Chi-square test - All caps vs Spam:")
print(chisq_caps)

#Visuals
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)

spam_counts = c(ham_count, spam_count)
names(spam_counts) = c("Ham", "Spam")
barplot(spam_counts, main = "Email Distribution", 
        col = c("lightblue", "lightcoral"), ylab = "Count")

links_spam_rate = tapply(as.numeric(as.character(data$is_spam)), data$links_category, mean)
barplot(links_spam_rate, main = "Spam Rate by Links Category", 
        ylab = "Spam Rate", col = "lightgreen")

offer_spam_rate = tapply(as.numeric(as.character(data$is_spam)), data$has_offer, mean)
names(offer_spam_rate) = c("No Offer", "Has Offer")
barplot(offer_spam_rate, main = "Spam Rate by Offer Presence", 
        ylab = "Spam Rate", col = "lightyellow")

caps_spam_rate = tapply(as.numeric(as.character(data$is_spam)), data$all_caps, mean)
names(caps_spam_rate) = c("No All Caps", "All Caps")
barplot(caps_spam_rate, main = "Spam Rate by All Caps", 
        ylab = "Spam Rate", col = "lightpink")

par(mfrow = c(1, 1))

#summary
cat("\nSUMMARY OF FINDINGS\n")
cat("1. Overall spam rate:", round(spam_rate, 4), "\n")
cat("2. 95% CI for spam rate: [", round(spam_ci$conf.int[1], 4), 
    ",", round(spam_ci$conf.int[2], 4), "]\n")

feature_effects = c(
  "Links" = max(links_spam_rate, na.rm = TRUE) - min(links_spam_rate, na.rm = TRUE),
  "Offer" = abs(offer_spam_rate[2] - offer_spam_rate[1]),
  "AllCaps" = abs(caps_spam_rate[2] - caps_spam_rate[1])
)

cat("3. Feature with highest spam rate variation:", 
    names(feature_effects)[which.max(feature_effects)], 
    "with difference of", round(max(feature_effects), 4), "\n")

cat("4. Statistical Significance (p < 0.05):\n")
cat("   - Offer Effect:", ifelse(prop_test_offer$p.value < 0.05, "Significant", "Not Significant"), "\n")
cat("   - Links Effect:", ifelse(chisq_links$p.value < 0.05, "Significant", "Not Significant"), "\n")
cat("   - All Caps Effect:", ifelse(chisq_caps$p.value < 0.05, "Significant", "Not Significant"), "\n")