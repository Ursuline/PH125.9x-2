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

# Feature engineering ####

# Create a 'Used' column to separate CL0 (never used) from all the others 
df.raw <- df.raw %>% mutate(Used = ifelse(Cannabis == "CL0", 0, 1))

# Change the Used predictor to a factor
df.raw[,'Used'] <- factor(as.character(df.raw[,'Used']))

dim(df.raw)
names(df.raw)
head(df.raw)
summary(df.raw)
str(df.raw)

# Check there are no missing values in the data set
anyNA(df.raw)
# Cannabis ####
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

# 78% used / 22% didn't
prop.table(table(df.train$Used))

# There are more responders that use cannabis daily than 
# responders that have never used
prop.table(table(df.train$Cannabis))

# Feature correlation analysis ####
df.cor <- df.train %>% select(Age, Gender, Education, Country, Ethnicity, 
                              Nscore, Escore, Oscore, Ascore, Cscore, 
                              Impulsive, SS)
cormat <- as_tibble(corr_plot(df.cor, "Feature correlation"))
cor.test(df.cor$SS, df.cor$Impulsive)
rm(df.cor)
# Most features are weakly correlated with one another, the strongest correlation is 
# between impulsivity and Sensation-seeking (62% correlation, p-value = 0)

# Frequency analysis

# Global plot parameters ####
fill <- 'skyblue3'
color <- 'grey'
fill_no <- "#af8dc3"
fill_yes <- "#7fbf7b"
used_colors <- c(fill_no, fill_yes) # No/Yes
alpha <- 0.4 # alpha-blending

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

# A: Demographic analyses #####
# 1. Age analysis ####

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

# 2. Gender analysis ####

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

# 3. Education analysis ####

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

# 4. Country analysis ####

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

# 5. Ethnicity analysis ####

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

# Demographic summary plots ####
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
             left = "Counts")
# Free up memory
rm(plotCountryProp, plotEduProp, plotEthProp, plotAgeProp, plotGenderProp)

# B: Psychological analyses #####
# 1. Neuroticism (N-score) plot ####
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
  scale_y_continuous(limits = c(0, .5))

# 2. Extraversion (E-score) plot ####
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
  scale_y_continuous(limits = c(0, .5))

# 3. Openness to experience (O-score) plot ####
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
  scale_y_continuous(limits = c(0, .5))

# 4. Agreeableness (A-score) plot ####

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
  scale_y_continuous(limits = c(0, .5))

# 5. Conscientiousness (C-score) plot ####
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
  scale_y_continuous(limits = c(0, .5)) 

# 6. Impulsiveness plot ####

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
  scale_y_continuous(limits = c(0, .5)) 

# 7. Sensation-seeking plot ####
SSDensityPlot <- df.train %>% 
  ggplot(aes(x = SS, fill = Used, color = I(color))) +
  geom_density(alpha = alpha) +
  labs(title = "Seeking sensations",
                     x = "Sensation-seeking score",
                     y = "") +
  scale_fill_manual(name = "Used?", 
                     values = c("0" = used_colors[1], "1" = used_colors[2]), 
                     labels = c("No", "Yes")) +
  scale_y_continuous(limits = c(0, .5)) 

# Psy Summary plot ####
grid.arrange(NscoreDensityPlot, EscoreDensityPlot, OscoreDensityPlot,
             AscoreDensityPlot, CscoreDensityPlot, ImpDensityPlot,
             SSDensityPlot,
             nrow = 3,
             top = "Cannabis use as a fuction of:",
             left = "Density"
)
