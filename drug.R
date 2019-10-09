# Load R environment ####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gplots)) install.packages("gplots", repos = "http://cran.us.r-project.org")
if(!require(graphics)) install.packages("graphics", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")


# Load data ####
# Remote / local flag
remote <- 1

if(remote == 1) { # Load data file from github repository
  df.raw <- read.table("https://raw.githubusercontent.com/Ursuline/PH125.9x-2/master/data/drug_consumption.csv", 
                       header = FALSE,
                       sep = ",")
} else { # Load local copy
  df.raw <- read.csv("./data/drug_consumption.csv", header = FALSE)
}
# Add a header row
names(df.raw) <- c("Id", "Age", "Gender", "Education", "Country", "Ethnicity",
                   "Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS",
                   "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis", "Choc", 
                   "Coke", "Crack", "Ecstasy", "Heroin", "Ketamine","Legalh", "LSD", 
                   "Meth", "Mushrooms", "Nicotine", "Semer", "VSA")

# A: Data engineering ####

#   Create 'Used' class ####
# Create a 'Used' column to separate CL0 (never used) from the others 
df.raw <- df.raw %>% mutate(Used = ifelse(Cannabis == "CL0", 0, 1))

# Change the Used predictor to a factor
df.raw[,'Used'] <- factor(as.character(df.raw[,'Used']))

# Keep Used as the only Class. 
df.raw <- df.raw %>% 
  select("Id", "Age", "Gender", "Education", "Country", "Ethnicity", "Nscore", 
         "Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS", 
         "Used")
#   Data partitioning ####
set.seed(15)
trainIndex <- createDataPartition(df.raw$Used, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)
df.train <- df.raw[ trainIndex,]
df.test  <- df.raw[-trainIndex,]
# B: Data exploration
#   Global plot parameters ####
fill <- 'skyblue3'
color <- 'grey'
fill_no <- "#af8dc3"
fill_yes <- "#7fbf7b"
used_colors <- c(fill_no, fill_yes) # No/Yes
alpha <- 0.4 # alpha-blending
axis_text_size <- 10
#   Class distribution plot ####
title <- paste("Frequency of cannabis use: ", 
               "training set (", as.character(dim(df.train)[1]), 
               "participants)")

plot.use <- df.train %>% 
  ggplot(aes(Used)) + 
  geom_histogram(stat = "count", 
                 aes(fill = I(fill), 
                     color = I(color))) +
  labs(title = title, x = "", y = "count") +
  scale_x_discrete(labels = c("Never used", "Used"))
plot.use
#   Balloon plot utility ####
balloon.plot <- function(cont, title){
  balloon_melted<-melt(cont, sort=F)
  ggplot(balloon_melted, 
         aes(x =Var2, y = Var1)) +
    geom_point( aes(size=value),
                shape = 21, 
                colour = color, 
                fill = fill)+
    theme(panel.background=element_blank(), 
          panel.border = element_rect(colour = color, fill = NA, size = 1))+
    scale_size_area(max_size=20) +
    labs(x="", y="", title = title)
}
#   Contingency plots prior to re-binning (balloon plots) ####
#     Age contingency plot ####
cont.age <- table(df.train$Age, df.train$Used)
colnames(cont.age) <- c("Not used", "Used")
rownames(cont.age) <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

balloon.plot(cont.age, "Age group")

#     Gender contingency plot ####
cont.gender <- table(df.train$Gender, df.train$Used)
colnames(cont.gender) <- c("Not used", "Used")
rownames(cont.gender) <- c("male", "female")

balloon.plot(cont.gender, "Gender")

#     Education contingency plot ####
cont.edu <- table(df.train$Education, df.train$Used)
colnames(cont.edu) <- c("Not used", "Used")
rownames(cont.edu) <- c("Left school before 16 yo", 
                        "Left school at 16 yo", 
                        "Left school at 17 yo", 
                        "Left school at 18 yo", 
                        "Some college/univ.", 
                        "Prof. certif./diploma",
                        "Univ. degree", 
                        "Masters degree",
                        "Doctorate degree")
balloon.plot(cont.edu, "Education")

#     Country contingency plot ####
cont.country <- table(df.train$Country, df.train$Used)
colnames(cont.country) <- c("Not used", "Used")
rownames(cont.country) <- c("USA", "New Zealand", "Other", "Australia", 
                            "Republic of Ireland", "Canada", "UK")
balloon.plot(cont.country, "Country")

#     Ethnicity contingency plot ####
cont.ethn <- table(df.train$Ethnicity, df.train$Used)
colnames(cont.ethn) <- c("Not used", "Used")
rownames(cont.ethn) <- c("Black", "Asian", "White", "Mixed-White/Black", 
                         "Other","Mixed-White/Asian", "Mixed-Black/Asian")
balloon.plot(cont.ethn, "Ethnicity")

#   Re-bin the data ####
df.train <- 
  df.train %>% 
  mutate(Country = ifelse(Country %in% c(-0.09765, 0.24923, -0.46841, 0.21128), -0.28519, Country)) %>%
  mutate(Age = ifelse(Age == 2.59171, 1.82213, Age)) %>%
  mutate(Education = ifelse(Education %in% c(-2.43591, -1.73790, -1.43719), -1.22751, # Dropped school
                            ifelse(Education == 1.98437, 1.16365, Education))) %>% # Merge MS & PhD 
  mutate(Ethnicity = ifelse(Ethnicity != -0.31685, 0.11440, Ethnicity))

df.test <- 
  df.test %>% 
  mutate(Country = ifelse(Country %in% c(-0.09765, 0.24923, -0.46841, 0.21128), -0.28519, Country)) %>%
  mutate(Age = ifelse(Age == 2.59171, 1.82213, Age)) %>%
  mutate(Education = ifelse(Education %in% c(-2.43591, -1.73790, -1.43719), -1.22751, # Dropped school
                            ifelse(Education == 1.98437, 1.16365, Education))) %>% # Merge MS & PhD 
  mutate(Ethnicity = ifelse(Ethnicity != -0.31685, 0.11440, Ethnicity))
# Contingency plot utility ####
demogPlot <- function(title, labels, x_axis_title){
  dP <- df.train %>%
    ggplot(aes(factor(.[,title]))) +
    geom_histogram(stat = "count",
                   position = "dodge",
                   aes(fill = Used)) +
    scale_x_discrete(labels = labels) +
    labs(title = title,
         x = x_axis_title,
         y = "") +
    scale_fill_manual(values = c("0" = used_colors[1],
                                 "1" = used_colors[2]),
                      labels = c("No", "Yes"),
                      guide = FALSE) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  return(dP)
}

# Contingency plots ####
#   Age contingency plot ####
title.age <- "Age"
labels.age <- c("18-24", "25-34", "35-44", "45-54", "55+")
plot.age <- demogPlot(title.age, labels.age, "years old")

#   Gender contingency plot ####
title.gender <- "Gender"
labels.gender <- c("male", "female")
plot.gender <- demogPlot(title.gender, labels.gender, "")

#   Education contingency plot ####
title.edu <- "Education"
labels.edu <- c("Left school as teen", 
                "Some college/univ.", 
                "Prof. certif./diploma",
                "Univ. degree", 
                "Graduate degree")
plot.edu <- demogPlot(title.edu, labels.edu, "")

#   Country contingency plot ####
title.country <- "Country"
labels.country <- c("USA", "Other", "UK")
plot.country <- demogPlot(title.country, labels.country, "")

#   Ethnicity contingency plot ####
title.ethn <- "Ethnicity"
labels.ethn <- c("White", "Non-white")
plot.ethn <- demogPlot(title.ethn, labels.ethn, "")

#   Combine the 5 contingency plots ####
grid.arrange(plot.country, plot.gender, plot.ethn,
             plot.age, plot.edu,
             layout_matrix = rbind(c(1, 1, 2, 2, 3, 3), 
                                   c(4, 4, 4, 5, 5, 5)),
             top = "Use of cannabis in training set by:",
             left = "Counts")

# Free up memory
rm(plot.country, plot.gender, plot.ethn, plot.age, plot.edu)
# Save environment as drugEnvironment.RData####
save.image(file='drugEnvironment.RData')