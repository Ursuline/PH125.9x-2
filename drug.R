if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

source("./correlation_matrix_plot.R")

# Data loader ####
df.raw <- read.csv("./data/drug_consumption.csv", header = FALSE)
names(df.raw) <- c("Id", "Age", "Gender", "Education", "Country", "Ethnicity",
                   "Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS",
                   "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", 
                   "Coke", "Crack", "Ecstasy", "Heroin", "Ketamine","Legalh", "LSD", 
                   "Meth", "Mushrooms", "Nicotine", "Semer", "VSA")


# DATA ANALYSIS & EXPLORATION

# Global plot parameters #####
fill <- 'skyblue3'
color <- 'grey'
fill_no <- "#af8dc3"
fill_yes <- "#7fbf7b"
used_colors <- c(fill_no, fill_yes) # No/Yes
alpha <- 0.4 # alpha-blending

# Feature engineering ####

# Create a 'Used' column to separate CL0 (never used) from all the others 
df.raw <- df.raw %>% mutate(Used = ifelse(Cannabis == "CL0", 0, 1))

# Change the Used predictor to a factor
df.raw[,'Used'] <- factor(as.character(df.raw[,'Used']))

# Remove the non-cannabis Classes 
df.raw <- df.raw %>% 
  select("Id", "Age", "Gender", "Education", "Country", "Ethnicity", "Nscore", 
         "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS", "Cannabis", 
         "Used")

dim(df.raw)
names(df.raw)
head(df.raw)
summary(df.raw)
str(df.raw)

# Check there are no missing values in the data set
anyNA(df.raw)

# Data partitioning ####
# Partition the data between training (80%) and test sets (20%) preserving
# the distribution of the Cannabis class
set.seed(15)
trainIndex <- createDataPartition(df.raw$Cannabis, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)

df.train <- df.raw[ trainIndex,]
df.test  <- df.raw[-trainIndex,]
dim(df.train)
dim(df.test)
str(df.train)
df.train %>% dim()

featurePlot(x=df.train[,1:4], y=df.train[,5], plot="pairs", auto.key=list(columns=3))
# 78% used / 22% didn't
prop.table(table(df.train$Used))

# There are more responders that use cannabis daily than 
# responders that have never used
prop.table(table(df.train$Cannabis))


# Feature correlation ####
df.cor <- df.train %>% select(Age, Gender, Education, Country, Ethnicity, 
                              Nscore, Escore, Oscore, Ascore, Cscore, 
                              Impulsive, SS)
cormat <- as_tibble(corr_plot(df.cor, "Feature correlation"))
cor.test(df.cor$SS, df.cor$Impulsive)
rm(df.cor)
# Most features are weakly correlated with one another, the strongest correlation is 
# between impulsivity and Sensation-seeking (62% correlation, p-value = 0)

# Frequency analysis

# Overall cannabis consumption ####

freq_labels <- c("Never used", "> a decade ago", "Last decade", 
                 "Last year", "Last month", "Last week", 
                 "Last day")

p1 <- df.train %>% 
  ggplot(aes(Cannabis)) + 
  geom_histogram(stat = "count", 
                 aes(fill = I(fill), 
                     color = I(color))) +
  labs(x = "Frequency of use", 
       y = "") +
  scale_x_discrete(labels = freq_labels) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))


p2 <- df.train %>% 
  ggplot(aes(Used)) + 
  geom_histogram(stat = "count", 
                 aes(fill = I(fill), 
                     color = I(color))) +
  labs(x = "", y = "") +
  scale_x_discrete(labels = c("Never used", "Used"))

top <- paste("Consumption of cannabis\n", "Training set:", as.character(dim(df.train)[1]), "participants")

grid.arrange(p2, p1, 
             nrow = 1,
             top = top,
             left = "Count"
)

# No use for the Cannabis column any longer
df.train <- df.train %>% select(-Cannabis)
df.test <- df.test %>% select(-Cannabis)

# A: Demographic analysis (before re-binning)####

#   1. Age ####

df.age <- df.train %>% group_by(Age) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Age) %>% 
  summarize(n = n()) %>% 
  filter(Used == "0") %>% 
  select(n)
df.age <- df.age %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.age <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

plotAgeProp <- df.age %>% 
  ggplot(aes(x = factor(Age), 
             y = prop_no)) + 
  geom_bar(stat = "identity", 
           aes(fill = I(fill_no), 
               color = I(color))) +
  scale_x_discrete(labels = labels.age)  +
  labs(title = "Age group",
       x = "Years old",
       y = "Proportion") +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1))

plotAge <- df.train %>% 
            ggplot(aes(factor(Age))) + 
            geom_histogram(stat = "count", 
                           position = "dodge",
                           aes(fill = Used)) +
            scale_x_discrete(labels = labels.age) +
            labs(title = "Age group",
                 x = "Years old",
                 y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE)

#   2. Gender ####

df.gender <- df.train %>% group_by(Gender) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Gender) %>% 
  summarize(n = n()) %>% 
  filter(Used == "0") %>% 
  select(n)
df.gender <- df.gender %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels <- c("Male", "Female")

plotGenderProp <- df.gender %>% 
  ggplot(aes(x = factor(Gender), y = prop_no)) + 
  geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
  scale_x_discrete(labels = labels)  +
  labs(title = "Gender",
       x = "",
       y = "Proportion") +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1))

plotGender <- df.train %>% 
  ggplot(aes(factor(Gender))) + 
  geom_histogram(stat = "count",  
                 position = "dodge",
                 aes(fill = Used)) +
  scale_x_discrete(labels = labels) +
  labs(title = "Gender",
       x = "",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], "1" = used_colors[2]), 
                    labels = c("No", "Yes")) +
  guides(fill = guide_legend(label.position = "left", 
                             label.hjust = 1))

#   3. Education ####

df.edu <- df.train %>% group_by(Education) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Education) %>% 
  summarize(n = n()) %>% 
  filter(Used == "0") %>% 
  select(n)
df.edu <- df.edu %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.edu <- c("Left school before 16 yo", 
            "Left school at 16 yo", 
            "Left school at 17 yo", 
            "Left school at 18 yo", 
            "Some college/univ.", 
            "Prof. certif./diploma",
            "Univ. degree", 
            "Masters degree",
            "Doctorate degree")

levels.edu <- c(-2.43591, -1.73790, -1.43719,
            -1.22751, -0.61113, -0.05921,
            0.45468, 1.16365, 1.98437)

plotEduProp <- df.edu %>% 
  ggplot(aes(x = factor(Education), 
             y = prop_no)) + 
  geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
  labs(title = "Education",
       x = "",
       y = "") +
  aes(x=reorder(factor(Education, 
                       labels = labels.edu), 
                -prop_no)) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.x=element_blank())

plotEdu <- df.train %>% 
  ggplot(aes(factor(Education))) + 
  geom_histogram(stat = "count", 
                 position = "dodge",
                 aes(fill = Used)) +
  scale_x_discrete(labels = labels.edu) +
  labs(title = "Level of education",
       x = "",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

#   4. Country ####

df.country <- df.train %>% group_by(Country) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Country) %>% 
  summarize(n = n()) %>% 
  filter(Used == "1") %>% # Handle the case of "New Zealand"
  select(n) %>% 
  mutate(n = df.country$n - n)
df.country <- df.country %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.ctry <- c("USA", "New Zealand", "Other", "Australia", 
            "Republic of Ireland", "Canada", "UK")

levels.ctry <- c(-0.57009, -0.46841, -0.28519, -0.09765, 
            0.21128, 0.24923, 0.96082)

plotCountryProp <- df.country %>% 
  ggplot(aes(x = factor(Country, 
                        levels = levels.ctry, 
                        labels = labels.ctry), 
             y = prop_no)) + 
  geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
  labs(title = "Country",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.x=element_blank())

plotCountry <- df.train %>% 
  ggplot(aes(factor(Country, 
                    labels = labels.ctry, 
                    levels = levels.ctry))) + 
  geom_histogram(stat = "count", 
                 position = "dodge",
                 aes(fill = Used)) +
  labs(title = "Country",
       x = "",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

#   5. Ethnicity ####

df.eth <- df.train %>% group_by(Ethnicity) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Ethnicity) %>% 
  summarize(n = n()) %>% 
  filter(Used == "1") %>% # Handle the case of "Mixed-Black/Asian"
  select(n) %>% 
  mutate(n = df.eth$n - n)
df.eth <- df.eth %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.eth <- c("Black", "Asian", "White", "Mixed-White/Black", 
            "Other","Mixed-White/Asian", "Mixed-Black/Asian")

levels.eth <- c(-1.10702, -0.50212, -0.31685, -0.22166, 
            0.11440, 0.12600, 1.90725)

plotEthProp <- df.eth %>% 
  ggplot(aes(x = factor(Ethnicity, 
                        levels = levels.eth, 
                        labels = labels.eth), 
             y = prop_no)) + 
  geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
  labs(title = "Ethnicity",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.x=element_blank())

plotEth <- df.train %>% 
  ggplot(aes(factor(Ethnicity, 
                    labels = labels.eth, 
                    levels = levels.eth))) + 
  geom_histogram(stat = "count", position = "dodge",
                 aes(fill = Used)) +
  labs(title = "Ethnicity",
       x = "",
       y = "")  +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_sqrt()

#   Demographic summary plots ####
# Distribution:
grid.arrange(plotCountry, plotEdu, plotEth,
             plotAge, plotGender,
             nrow = 2,
             top = "Use of cannabis in training set by:",
             left = "Counts")

# Free up memory (but keep plotGender for after binning plots)
rm(plotCountry, plotEdu, plotEth, plotAge)

# Proportions:
grid.arrange(plotCountryProp, plotEduProp, plotEthProp,
             plotAgeProp, plotGenderProp,
             nrow = 2,
             top = "Proportion of cannabis non-users in training set by:",
             left = "Counts", 
             bottom = "Higher value = lower use")
# Free up memory (but keep plotGenderProp for after binning plots)
rm(plotCountryProp, plotEduProp, plotEthProp, plotAgeProp)

#   Rebin the demographic predictors ####
# Country-  USA, UK, Other_Ctry
# Age - lump 65+ into 55+
# Education - bin all teenage left school together
# Ethnicity - 2 bins: whites and others

df.train <- 
  df.train %>% 
  mutate(Country = ifelse(Country %in% c(-0.09765, 0.24923, -0.46841, 0.21128), -0.28519, Country)) %>%
  mutate(Age = ifelse(Age == 2.59171, 1.82213, Age)) %>%
  mutate(Education = ifelse(Education %in% c(-2.43591, -1.73790, -1.43719), -1.22751, Education)) %>%
  mutate(Ethnicity = ifelse(Ethnicity != -0.31685, 0.11440, Ethnicity))

df.test <- 
  df.test %>% 
  mutate(Country = ifelse(Country %in% c(-0.09765, 0.24923, -0.46841, 0.21128), -0.28519, Country)) %>%
  mutate(Age = ifelse(Age == 2.59171, 1.82213, Age)) %>%
  mutate(Education = ifelse(Education %in% c(-2.43591, -1.73790, -1.43719), -1.22751, Education)) %>%
  mutate(Ethnicity = ifelse(Ethnicity != -0.31685, 0.11440, Ethnicity))



# B: Demographic analysis (after re-binning)
#   1. Age ####

df.age <- df.train %>% group_by(Age) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Age) %>% 
  summarize(n = n()) %>% 
  filter(Used == "0") %>% 
  select(n)
df.age <- df.age %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.age <- c("18-24", "25-34", "35-44", "45-54", "55+")

plotAgeProp <- df.age %>% 
  ggplot(aes(x = factor(Age), 
             y = prop_no)) + 
  geom_bar(stat = "identity", 
           aes(fill = I(fill_no), 
               color = I(color))) +
  scale_x_discrete(labels = labels.age)  +
  labs(title = "Age group",
       x = "Years old",
       y = "Proportion") +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) 

plotAge <- df.train %>% 
  ggplot(aes(factor(Age))) + 
  geom_histogram(stat = "count", 
                 position = "dodge",
                 aes(fill = Used)) +
  scale_x_discrete(labels = labels.age) +
  labs(title = "Age group",
       x = "Years old",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) 

#   2. Education ####

df.edu <- df.train %>% group_by(Education) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Education) %>% 
  summarize(n = n()) %>% 
  filter(Used == "0") %>% 
  select(n)
df.edu <- df.edu %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.edu <- c("Left school as teen", 
                "Some college/univ.", 
                "Prof. certif./diploma",
                "Univ. degree", 
                "Masters",
                "Doctorate")

plotEduProp <- df.edu %>% 
  ggplot(aes(x = factor(Education, labels = labels.edu), 
             y = prop_no)) + 
  geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
  labs(title = "Education",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.x=element_blank())

plotEdu <- df.train %>% 
  ggplot(aes(factor(Education))) + 
  geom_histogram(stat = "count", 
                 position = "dodge",
                 aes(fill = Used)) +
  scale_x_discrete(labels = labels.edu) +
  labs(title = "Level of education",
       x = "",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

#   3. Country ####

df.country <- df.train %>% group_by(Country) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Country) %>% 
  summarize(n = n()) %>% 
  filter(Used == "1") %>% # Handle the case of "New Zealand"
  select(n) %>% 
  mutate(n = df.country$n - n)
df.country <- df.country %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.ctry <- c("USA", "New Zealand", "Other", "Australia", 
                 "Republic of Ireland", "Canada", "UK")

levels.ctry <- c(-0.57009, -0.46841, -0.28519, -0.09765, 
                 0.21128, 0.24923, 0.96082)

plotCountryProp <- df.country %>% 
  ggplot(aes(x = factor(Country, 
                        levels = levels.ctry, 
                        labels = labels.ctry), 
             y = prop_no)) + 
  geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
  labs(title = "Country",
       x = "",
       y = "") +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.x=element_blank())

plotCountry <- df.train %>% 
  ggplot(aes(factor(Country, 
                    labels = labels.ctry, 
                    levels = levels.ctry))) + 
  geom_histogram(stat = "count", 
                 position = "dodge",
                 aes(fill = Used)) +
  labs(title = "Country",
       x = "",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE)

#   4. Ethnicity ####

df.eth <- df.train %>% group_by(Ethnicity) %>% summarize(n = n())
temp <- df.train %>% 
  group_by(Used, Ethnicity) %>% 
  summarize(n = n()) %>% 
  filter(Used == "1") %>% # Handle the case of "Mixed-Black/Asian"
  select(n) %>% 
  mutate(n = df.eth$n - n)
df.eth <- df.eth %>% mutate(No = temp$n, prop_no = No/n)

# Plot
labels.eth <- c("Black", "Asian", "White", "Mixed-White/Black", 
                "Other","Mixed-White/Asian", "Mixed-Black/Asian")

levels.eth <- c(-1.10702, -0.50212, -0.31685, -0.22166, 
                0.11440, 0.12600, 1.90725)

plotEthProp <- df.eth %>% 
  ggplot(aes(x = factor(Ethnicity, 
                        levels = levels.eth, 
                        labels = labels.eth), 
             y = prop_no)) + 
  geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
  labs(title = "Ethnicity",
       x = "",
       y = "") +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(axis.title.x=element_blank())

plotEth <- df.train %>% 
  ggplot(aes(factor(Ethnicity, 
                    labels = labels.eth, 
                    levels = levels.eth))) + 
  geom_histogram(stat = "count", position = "dodge",
                 aes(fill = Used)) +
  labs(title = "Ethnicity",
       x = "",
       y = "")  +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"), 
                    guide = FALSE) 
#   Demographics summary plots ####
# Distribution:
grid.arrange(plotCountry, plotEdu, plotEth,
             plotAge, plotGender,
             nrow = 2,
             top = "Use of cannabis in training set by:",
             left = "Counts")

# Free up memory 
rm(plotCountry, plotEdu, plotEth, plotAge, plotGender)

# Proportions:
grid.arrange(plotCountryProp, plotEduProp, plotEthProp,
             plotAgeProp, plotGenderProp,
             nrow = 2,
             top = "Proportion of cannabis non-users in training set by:",
             left = "Counts", 
             bottom = "Higher value = lower use")

# Free up memory 
rm(plotCountryProp, plotEduProp, plotEthProp, plotAgeProp, plotAgeProp)

# C: Personality analyses
breaks <- seq(-3, 3, .5)
angle <- 35
#   1. Neuroticism (N-score)  ####

mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Nscore))

NscoreDensityPlot <- df.train %>% 
  ggplot(aes(x = Nscore, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Neuroticism",
       x = "N-score",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"),
                    guide = FALSE) +
  scale_y_continuous(limits = c(0, .5))  +
  scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2])+
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#   2. Extraversion (E-score)  ####

mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Escore))

EscoreDensityPlot <- df.train %>% 
  ggplot(aes(x = Escore, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Extraversion",
       x = "E-score",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"),
                    guide = FALSE) +
  scale_y_continuous(limits = c(0, .5))  +
  scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2])+
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#   3. Openness to experience (O-score)  ####
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Oscore))

OscoreDensityPlot <- df.train %>% 
  ggplot(aes(x = Oscore, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Openness to experience",
       x = "O-score",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"),
                    guide = FALSE) +
  scale_y_continuous(limits = c(0, .5))  +
  scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2]) +
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#   4. Agreeableness (A-score) ####
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Ascore))

AscoreDensityPlot <- df.train %>% 
  ggplot(aes(x = Ascore, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Agreeableness",
       x = "A-score",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"),
                    guide = FALSE) +
  scale_y_continuous(limits = c(0, .5))  +
  scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2]) +
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#   5. Conscientiousness (C-score) ####
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Cscore))

CscoreDensityPlot <- df.train %>% 
  ggplot(aes(x = Cscore, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Conscientiousness",
       x = "C-score",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"),
                    guide = FALSE) +
  scale_y_continuous(limits = c(0, .5)) +
  scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2]) +
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#   6. Impulsiveness ####
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Impulsive))

ImpDensityPlot <- df.train %>% 
  ggplot(aes(x = Impulsive, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Impulsivity",
       x = "Impulsivity score",
       y = "") +
  scale_fill_manual(name = "Used?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes"),
                    guide = FALSE) +
  scale_y_continuous(limits = c(0, .5)) +
  scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2]) +
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#   7. Sensation-seeking ####
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(SS))

SSDensityPlot <- df.train %>% 
  ggplot(aes(x = SS, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Seeking sensations",
                     x = "Sensation-seeking score",
                     y = "") +
  scale_fill_manual(name = "Used?", 
                     values = c("0" = used_colors[1], "1" = used_colors[2]), 
                     labels = c("No", "Yes")) +
  scale_y_continuous(limits = c(0, .5)) +
  scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
  geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2]) +
  theme(axis.text.x = element_text(angle = angle, hjust = 1))

#   Personality summary plot ####
# grid.arrange(NscoreDensityPlot, EscoreDensityPlot, OscoreDensityPlot,
#              AscoreDensityPlot, CscoreDensityPlot, ImpDensityPlot,
#              SSDensityPlot,
#              nrow = 3,
#              top = "Cannabis use as a fuction of:",
#              left = "Density"
# )
grid.arrange(arrangeGrob(NscoreDensityPlot, EscoreDensityPlot, OscoreDensityPlot,
                         AscoreDensityPlot, CscoreDensityPlot, ImpDensityPlot, nrow=3), 
             SSDensityPlot, heights=c(3/4, 1/4), ncol=1, nrow=2)
# Free up memory
rm(NscoreDensityPlot, EscoreDensityPlot, OscoreDensityPlot, AscoreDensityPlot, 
   CscoreDensityPlot, ImpDensityPlot, SSDensityPlot)

# MODELING ####

# Feature selection with recursive feature elimination #####
names(df.train)
names(df.test)
# Wrapper for the RFE algorithm
rfe_drug <- function(df, outcomeName){ 
  # Remove the id column
  df <- df %>% select(-Id) 

  #Make the Used class a factor
  df$Used <- factor(df$Used,
                     levels = c(0, 1), 
                     labels = c("0", "1"))

  # RFE controls
  control <- rfeControl(functions = rfFuncs,
                        method = "repeatedcv",
                        repeats = 10,
                        verbose = TRUE)
  predictors <- names(df)[!names(df) %in% outcomeName]

  # RFE Call
  pred_Profile <- rfe(df[ ,predictors], 
                      unlist(df[ ,outcomeName]), 
                      rfeControl = control)
  print(predictors)
  return(pred_Profile)
}

# RFE call
print("*** Performing RFE ***")
outcomeName <- "Used"
set.seed(5)
rfe_Profile <- rfe_drug(df.train, outcomeName)
print("*** RFE completed ***")
rfe_Profile
importance <- varImp(rfe_Profile, scale = FALSE)
# summarize importance
print(importance)

# Wven though the RFE did not eliminate any predictors, the low relative importance of 
# N-score and E-Score is in agreement with what we observed in the data. 
# plot importance
plot(importance)

plot(rfe_Profile, metric = "Rsquared")

plot(rfe_Profile, type=c("g", "o"), cex = 1.0, col = 1:11)

anyNA(df.train)
anyNA(df.test)

predictors <- predictors(rfe_Profile)
length(predictors)

display_model <- function(model, plot_title){
  print(model)
  plot(varImp(object = model), 
       main = plot_title, 
       top = 12)
}

# Prediction -> get rid of this
run_prediction_old <- function(df.test, model, predictors) {
  predictions <- predict.train(object = model, 
                               df.test[,predictors], 
                               type="raw")

  print(table(predictions))
  df.test <- df.test %>% mutate(predictions = predictions)

  df.test <- df.test %>% 
    mutate(predictions = as.integer(as.character(predictions)))
  return(df.test)
}

confMat <- function(model, df.test){
  return(confusionMatrix(predict(model, newdata = df.test), df.test$Used))
  CM.glm.base$overall["Accuracy"]
}

# Training parameters
fitControl <- trainControl( 
  method = "repeatedcv", # Repeated k-fold Cross-Validation
  number = 10, #10-fold CV
  repeats = 10, # 10 repeats
  allowParallel = TRUE
)

# Generalized linear model ####
set.seed(50)
model.glm <- train(df.train[,predictors], 
                          as.factor(df.train[,outcomeName]), 
                          method = 'glm', 
                          metric = 'Accuracy')

# Generalized Linear Model
display_model(model.glm, "GLM - Variable Importance")
max(model.glm$results$Accuracy) # Fit to training data

CM.glm <- confusionMatrix(predict(model.glm, newdata = df.test), df.test$Used)
CM.glm$overall["Accuracy"] # Fit to test data

# Generalized linear model with penalized maximum likelihood ####
# GLMnet without parameter tuning
set.seed(35)
model.glmnet.base <- train(df.train[,predictors], 
                   as.factor(df.train[,outcomeName]), 
                   method = 'glmnet', 
                   metric = 'Accuracy')

# Generalized Linear Model
display_model(model.glmnet.base, "GLMnet - Variable Importance (no tuning)")
max(model.glmnet.base$results$Accuracy) # Fit to training data
model.glmnet.base

CM.glmnet.base <- confusionMatrix(predict(model.glmnet.base, newdata = df.test), df.test$Used)
CM.glmnet.base$overall["Accuracy"] # Fit to test data

# GLMnet with parameter tuning
grid.glmnet <- expand.grid(
  alpha = seq(from = 0, to = .15, length = 25),
  lambda = seq(0, .01, length = 25)
)
set.seed(35)
model.glmnet <- train(df.train[,predictors],
                     as.factor(df.train[,outcomeName]), 
                     method = "glmnet",
                     trControl = fitControl,
                     tuneGrid = grid.glmnet, 
                     metric = "Accuracy")
display_model(model.glmnet, "GLMnet - Variable Importance")
model.glmnet
max(model.glmnet$results$Accuracy) # Fit to training data

CM.glmnet <- confusionMatrix(predict(model.glmnet, newdata = df.test), df.test$Used)
CM.glmnet$overall["Accuracy"] # Fit to test data

# Decision trees ####
set.seed(90)
# RPART Without parameter tuning
model.rpart.base <- train(df.train[,predictors], 
                          as.factor(df.train[,outcomeName]), 
                          method = 'rpart', 
                          metric = 'Accuracy')

display_model(model.rpart.base, "Decision Tree - Variable Importance (no parameter tuning)")
model.rpart.base
max(model.rpart.base$results$Accuracy) # Fit to training data

CM.rpart.base <- confusionMatrix(predict(model.rpart.base, newdata = df.test), df.test$Used)
CM.rpart.base$overall["Accuracy"] # Fit to test data

set.seed(80)
# RPART With parameter tuning 
model.rpart <- train(df.train[,predictors],
                     as.factor(df.train[,outcomeName]), 
                     method = "rpart",
                     trControl = fitControl,
                     tuneLength = 500, 
                     parms = list(split = 'information'))

display_model(model.rpart, "Decision Tree - Variable Importance")
model.rpart
max(model.rpart$results$Accuracy) # Fit to training data

CM.rpart <- confusionMatrix(predict(model.rpart, newdata = df.test), df.test$Used)
CM.rpart$overall["Accuracy"] # Fit to test data


# Random forest ####
set.seed(25)
# RF without parameter tuning
model.rf.base <- train(df.train[,predictors], 
                          as.factor(df.train[,outcomeName]), 
                          method = 'rf', 
                          metric = 'Accuracy')

display_model(model.rf.base, "Random forest - Variable Importance (no parameter tuning)")
model.rf.base
max(model.rf.base$results$Accuracy) # Fit to training data

CM.rf.base <- confusionMatrix(predict(model.rf.base, newdata = df.test), df.test$Used)
CM.rf.base$overall["Accuracy"] # Fit to test data

# RF with parameter tuning
grid.rf <- expand.grid(mtry = seq(1, 10))
set.seed(25)
model.rf <- train(df.train[,predictors], 
                  as.factor(df.train[,outcomeName]),
                  method = 'rf',
                  data = df.train,
                  tuneGrid = grid.rf)
display_model(model.rf, "RF - Variable Importance")
model.rf
max(model.rf$results$Accuracy) # Fit to training data

CM.rf <- confusionMatrix(predict(model.rf, newdata = df.test), df.test$Used)
CM.rf$overall["Accuracy"] # Fit to test data

# Stochastic Gradient Boosting ####
# GBM without parameter tuning
set.seed(5)
model.gbm.base <- train(df.train[,predictors], 
                       as.factor(df.train[,outcomeName]), 
                       method = 'gbm', 
                       metric = 'Accuracy')
display_model(model.gbm.base, "GBM - Variable Importance (no parameter tuning)")
max(model.gbm.base$results$Accuracy)

CM.gbm.base <- confusionMatrix(predict(model.gbm.base, newdata = df.test), df.test$Used)
CM.gbm.base$overall["Accuracy"] # Fit to test data

# GBM with parameter tuning
max.depth <- floor(sqrt(NCOL(df.train)))
# The grid values below were determined after many iterations
grid.gbm <- expand.grid(n.trees = seq(195, 218, 1),
                        shrinkage = seq(.01, .04, .01),
                        n.minobsinnode = 8,
                        interaction.depth = 3)
set.seed(5)
model.gbm <- train(df.train[,predictors],
                   as.factor(df.train[,outcomeName]),
                   method = 'gbm',
                   trControl = fitControl,
                   tuneGrid = grid.gbm)
display_model(model.gbm, "GBM - Variable Importance")
model.gbm
max(model.gbm$results$Accuracy) # Fit to training data

CM.gbm <- confusionMatrix(predict(model.gbm, newdata = df.test), df.test$Used)
CM.gbm$overall["Accuracy"] # Fit to test data


# Neural network ####
# NNET without parameter tuning
set.seed(125)
model.nnet.base <- train(df.train[,predictors], 
                        as.factor(df.train[,outcomeName]), 
                        method = 'nnet', 
                        metric = 'Accuracy')
display_model(model.nnet.base, "Neural network - Variable Importance (no parameter tuning)")
max(model.nnet.base$results$Accuracy) # Fit to training data
model.nnet.base

CM.nnet.base <- confusionMatrix(predict(model.nnet.base, newdata = df.test), df.test$Used)
CM.nnet.base$overall["Accuracy"] # Fit to test data

# NNET with parameter tuning
grid.nnet <- expand.grid(size = c(1:6),
                         decay = seq(0.1, 0.5, 0.1))
set.seed(125)
model.nnet <- train(df.train[,predictors],
                   as.factor(df.train[,outcomeName]),
                   method = 'nnet',
                   trControl = fitControl,
                   tuneGrid = grid.nnet)
display_model(model.nnet, "NNET - Variable Importance")
max(model.nnet$results$Accuracy) # Fit to training data
model.nnet

CM.nnet <- confusionMatrix(predict(model.nnet, newdata = df.test), df.test$Used)
CM.nnet$overall["Accuracy"] # Fit to test data

# Model summary table ####

# Compare base model performances using resample()
models.compare.base <- resamples(list(GLM = model.glm.base, RPART = model.rpart.base, RF = model.rf.base, GBM = model.gbm.base, NNET = model.nnet.base))
# Summary of the models performances
summary(models.compare.base)

model.fit <- tibble(
  method = c("GLM", 
             "GLMnet (no tuning)", "GLMnet",
             "RPART (no tuning)", "RPART", 
             "RF (no tuning)", "RF", 
             "GBM (no tuning)", "GBM",
             "NNET (no tuning)", "NNET"),
  train = c(max(model.glm$results$Accuracy),
            max(model.glmnet.base$results$Accuracy), max(model.glmnet$results$Accuracy),
            max(model.rpart.base$results$Accuracy), max(model.rpart$results$Accuracy), 
            max(model.rf.base$results$Accuracy), max(model.rf$results$Accuracy), 
            max(model.gbm.base$results$Accuracy), max(model.gbm$results$Accuracy),
            max(model.nnet.base$results$Accuracy), max(model.nnet$results$Accuracy)),
  test = c(CM.glm$overall["Accuracy"], 
           CM.glmnet.base$overall["Accuracy"], CM.glmnet$overall["Accuracy"],
           CM.rpart.base$overall["Accuracy"], CM.rpart$overall["Accuracy"], 
           CM.rf.base$overall["Accuracy"], CM.rf$overall["Accuracy"],
           CM.gbm.base$overall["Accuracy"], CM.gbm$overall["Accuracy"],
           CM.nnet.base$overall["Accuracy"], CM.nnet$overall["Accuracy"]))

model.fit %>% ggplot(aes(reorder(x = method, test), y = test)) + 
  geom_bar(stat = "identity", aes(color = I(color), fill = I(fill))) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(title = "Model comparison",
       x = "",
       y = "Fit to test set")+ 
  geom_text(aes(label = sprintf("%0.2f%%", round(test*100, digits = 1)), hjust = 1.25),  
            size=4, 
            color = "white") +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = scales::percent_format(accuracy = 1)) + 
  coord_flip()

save.image(file='environment.RData')

# Grading Rubric ####
# Files (5 points possible)
# The appropriate files are submitted in the correct formats: a report in both PDF and Rmd format and an R script in R format.
# 
# 0 points: No files provided
# 3 points: At least one file is missing and/or not in the correct format
# 5 points: All 3 files were submitted in the requested formats
# Report (25 points possible)
# The report documents the analysis and presents the findings, along with supporting statistics and figures. The report must be written in English and uploaded. The report must include at least the following sections:
#   
#   an introduction/overview/executive summary section that describes the dataset and summarizes the goal of the project and key steps that were performed
# a methods/analysis section that explains the process and techniques used, such as data cleaning, data exploration and visualization, any insights gained, and your modeling approach
# a results section that presents the modeling results and discusses the model performance
# a conclusion section that gives a brief summary of the report, its limitations, and future work (the last two are recommended but not necessary)
# 0 points: The report is either not uploaded or contains very minimal information OR the report appears to violate the terms of the edX Honor Code.
# 5 points: One or more required sections of the report are missing.
# 10 points: The report includes all required sections, but the report is significantly difficult to follow or missing significant supporting detail in multiple sections.
# 15 points: The report includes all required sections, but the report is difficult to follow or missing supporting detail in one section (or has minor flaws in multiple sections).
# 20 points: The report includes all required sections and is easy to follow, but with minor flaws in one section.
# 25 points: The report includes all required sections, is easy to follow with good supporting detail throughout, and is insightful and innovative.
# Code (20 points)
# The code in the R script should run without errors and should be well-commented and easy to follow. It should also use relative file paths and automatically install missing packages. The dataset you use should either be automatically be downloaded by your code or provided in your GitHub repo.
# 
# 0 points: Code does not run and produces many errors OR code appears to violate the terms of the edX Honor Code.
# 5 points: Code runs but does not produce output consistent with what is presented in the report OR there is overtraining (the test set is used for training steps).
# 10 points: Code runs but is difficult to follow and/or may not produce output entirely consistent with what is presented in the report.
# 15 points: Code runs, can be followed, is at least mostly consistent with the report, but is lacking (sufficient) comments and explanation OR uses absolute paths instead of relative paths OR does not automatically install missing packages OR does not provide easy access to the dataset (either via automatic download or inclusion in a GitHub repository).
# 20 points: Code runs easily, is easy to follow, is consistent with the report, and is well-commented. All file paths are relative and missing packages are automatically installed with if(!require) statements.
