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
df.raw <- df.raw %>% mutate(Used = ifelse(Cannabis %in% c("CL0", "CL1", "CL2"), 0, 1))

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
# B: Data exploration ####
#     Global plot parameters ####
fill <- 'skyblue3'
color <- 'grey'
fill_no <- "#af8dc3"
fill_yes <- "#7fbf7b"
used_colors <- c(fill_no, fill_yes) # No/Yes
alpha <- 0.4 # alpha-blending
axis_text_size <- 10
#   1. Class distribution plot ####
title <- paste("Frequency of cannabis use: training set (",
               as.character(dim(df.train)[1]), 
               " participants)", 
               sep = '') # prevent blank space

plot.use <- df.train %>% 
  ggplot(aes(Used)) + 
  geom_histogram(stat = "count", 
                 aes(fill = I(fill), 
                     color = I(color))) +
  labs(title = title, x = "", y = "count") +
  scale_x_discrete(labels = c("Never used", "Used"))
plot.use
#   2. Contingency plots prior to re-binning (balloon plots) ####
#     Balloon plot utility ####
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
#     a. Age balloon plot ####
cont.age <- table(df.train$Age, df.train$Used)
colnames(cont.age) <- c("Not used", "Used")
rownames(cont.age) <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

plot.balloon.age <- balloon.plot(cont.age, "Age group")

#     b. Gender balloon plot ####
cont.gender <- table(df.train$Gender, df.train$Used)
colnames(cont.gender) <- c("Not used", "Used")
rownames(cont.gender) <- c("male", "female")

plot.balloon.gender <- balloon.plot(cont.gender, "Gender")

#     c. Education balloon plot ####
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

plot.balloon.edu <- balloon.plot(cont.edu, "Education")

#     d. Country balloon plot ####
cont.country <- table(df.train$Country, df.train$Used)
colnames(cont.country) <- c("Not used", "Used")
rownames(cont.country) <- c("USA", "New Zealand", "Other", "Australia", 
                            "Republic of Ireland", "Canada", "UK")

plot.balloon.country <- balloon.plot(cont.country, "Country")

#     e. Ethnicity balloon plot ####
cont.ethn <- table(df.train$Ethnicity, df.train$Used)
colnames(cont.ethn) <- c("Not used", "Used")
rownames(cont.ethn) <- c("Black", "Asian", "White", "Mixed-White/Black", 
                         "Other","Mixed-White/Asian", "Mixed-Black/Asian")
plot.balloon.ethn <- balloon.plot(cont.ethn, "Ethnicity")

#     Re-bin the data ####
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
#   3. Analysis of demographics ####
#     Contingency plots ####
#       Contingency plot utility ####
demogPlot <- function(title, labels, x_axis_title, legend){
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
  if (legend == "yes") {
    dP <- dP + 
      scale_fill_manual(name = "User?",
                        values = c("0" = used_colors[1], "1" = used_colors[2]), 
                        labels = c("No", "Yes"))
  }
  return(dP)
}
#       a. Age  ####
title.age <- "Age"
labels.age <- c("18-24", "25-34", "35-44", "45-54", "55+")
plot.age <- demogPlot(title.age, labels.age, "years old", "no")

#       b. Gender ####
title.gender <- "Gender"
labels.gender <- c("male", "female")
plot.gender <- demogPlot(title.gender, labels.gender, "", "no")

#       c. Education ####
title.edu <- "Education"
labels.edu <- c("Left school as teen", 
                "Some college/univ.", 
                "Prof. certif./diploma",
                "Univ. degree", 
                "Graduate degree")
plot.edu <- demogPlot(title.edu, labels.edu, "", "yes")

#       d. Country ####
title.country <- "Country"
labels.country <- c("USA", "Other", "UK")
plot.country <- demogPlot(title.country, labels.country, "", "no")

#       e. Ethnicity ####
title.ethn <- "Ethnicity"
labels.ethn <- c("White", "Non-white")
plot.ethn <- demogPlot(title.ethn, labels.ethn, "", "no")

#     Ratios ####
#       Ratios utilities ####
propPlot <- function(df, labels, title, maxy){
  plot <- df %>%
    ggplot(aes(reorder(Var, -Prop), Prop)) +
    geom_bar(stat = "identity", 
             aes(fill = I(fill_no), 
                 color = I(color))
             ) +
    labs(title = title,
         x = "",
         y = "") +
    theme(text = element_text(size = axis_text_size)) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    geom_text(aes(label = sprintf("%0.1f", Prop), 
                  hjust = .4,
                  vjust =-1),
              size = 3,
              color = "grey30") +
    scale_y_continuous(limits = c(0, maxy))
  return(plot)
}
propTable <- function(pred, labels){
  df.prop <- df.train %>% 
    group_by_at(vars(pred, "Used")) %>% 
    summarize(n = n()) %>% 
    filter(Used == "0") %>% 
    select(pred, Never_Used = n)
  temp <- df.train %>% 
    group_by_at(vars(pred, "Used")) %>% 
    summarize(n = n()) %>% 
    filter(Used == "1") %>% 
    select(Used = n)
  df.prop <- cbind(df.prop, temp) %>% 
    select(pred, Never_Used, Used) %>% 
    mutate(Prop = Used/Never_Used) %>% 
    arrange(!! rlang::sym(c(pred))) # sort by predictor value
  df.prop <- cbind(Var = labels, df.prop)
  return(df.prop)
}
#       a. Age ####
table.age <- propTable('Age', labels.age)
plot.prop.age <- propPlot(table.age, labels.age, "Age", 12)

#       b. Gender ####
table.gender <- propTable('Gender', labels.gender)
plot.prop.gender <- propPlot(table.gender, labels.gender, "Gender", 8)

#       c. Education ####
table.edu <- propTable('Education', labels.edu)
plot.prop.edu <- propPlot(table.edu, labels.edu, "Education", 20)

#       d. Country ####
table.country <- propTable('Country', labels.country)
plot.prop.country <- propPlot(table.country, labels.country, "Country", 32)

(table.country %>% filter(Var == "USA"))$Prop

#       e. Ethnicity ####
table.ethn <- propTable('Ethnicity', labels.ethn)
plot.prop.ethn <- propPlot(table.ethn, labels.ethn, "Ethnicity", 5)

#   4. Personality analysis ####
#     Personality analysis plot parameters ####
breaks <- seq(-3, 3, .75)
angle <- 60
jitter_width <- 0.02
jitter_size <- 1
#     Personality analysis plot utilities ####
plot.box.personality <- function(df, feature, title, y.axis.title) {
  plot <- df %>% 
    ggplot(aes(y = .[,feature], x = Used, fill = Used, color = I(color))) +
    geom_boxplot()  +
    labs(title = title,
         y = y.axis.title,
         x = "") +
    scale_fill_manual(values = c("0" = used_colors[1], 
                                 "1" = used_colors[2]), 
                      labels = c("No", "Yes")) +
    scale_y_continuous(limits = c(-3, 3), breaks = breaks)+
    scale_x_discrete(labels = c("No", "Yes")) + 
    geom_jitter(width = jitter_width, size = jitter_size)
  return(plot)
}

plot.density.personality <- function(df, feature, title, x.axis.title) {
  plot <- df %>% 
    ggplot(aes(x = .[,feature], fill = Used, color = I(color))) +
    geom_density(alpha = alpha) +
    labs(title = title,
         x = x.axis.title,
         y = "") +
    scale_fill_manual(name = "User?", 
                      values = c("0" = used_colors[1], 
                                 "1" = used_colors[2]), 
                      labels = c("No", "Yes")) +
    scale_y_continuous(limits = c(0, .5)) +
    scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
    geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
    geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2])+
    theme(axis.text.x = element_text(angle = angle, hjust = 1))
  return(plot)
}
#     a. Neuroticism ####
# Neuroticism (N-score) plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Nscore))

# Box plot
feature.Nscore <- "Nscore"
title.Nscore <- "Neuroticism"
title.y.Nscore <- "N-score"
plot.box.Nscore <- 
  plot.box.personality(df.train, feature.Nscore, title.Nscore, title.y.Nscore)

# Density plot
plot.density.Nscore <- 
  plot.density.personality(df.train, feature.Nscore, title.Nscore, title.y.Nscore)

# Are they normally distributed ?
# Shapiro-Wilk normality test for Not Used
shapiro.Nscore.notUsed <- with(df.train, shapiro.test(Nscore[Used == "0"]))
# Shapiro-Wilk normality test for Used
shapiro.Nscore.Used <- with(df.train, shapiro.test(Nscore[Used == "1"]))

# Are they identical?
# Mann-Whitney-Wilcoxon test:
wilcox.Nscore <- with(df.train, wilcox.test(Nscore[Used == "0"], Nscore[Used == "1"]))

#     b. Extraversion ####
# Extraversion (E-score) plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Escore))

# Box plot
feature.Escore <- "Escore"
title.Escore <- "Extraversion"
title.y.Escore <- "E-score"
plot.box.Escore <- 
  plot.box.personality(df.train, feature.Escore, title.Escore, title.y.Escore)

# Density plot
plot.density.Escore <- 
  plot.density.personality(df.train, feature.Escore, title.Escore, title.y.Escore)

# Are they normally distributed ?
# Shapiro-Wilk normality test for Not Used
shapiro.Escore.notUsed <- with(df.train, shapiro.test(Escore[Used == "0"]))
# Shapiro-Wilk normality test for Used
shapiro.Escore.Used <- with(df.train, shapiro.test(Escore[Used == "1"]))

# Are they identical?
# Student t-test (mean)
t_test.Escore <- 
  with(df.train, t.test(Escore[Used == "0"], Escore[Used == "1"], var.equal = FALSE))
# Student t-test (variance)
t_test.var.Escore <- 
  with(df.train, t.test(Escore[Used == "0"], Escore[Used == "1"], var.equal = TRUE))

#     c. Openness to experience ####
# Openness to experience (O-score) plot 
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Oscore))

# Box plot
feature.Oscore <- "Oscore"
title.Oscore <- "Openness to experience"
title.y.Oscore <- "O-score"
plot.box.Oscore <- 
  plot.box.personality(df.train, feature.Oscore, title.Oscore, title.y.Oscore)

# Density plot
plot.density.Oscore <- 
  plot.density.personality(df.train, feature.Oscore, title.Oscore, title.y.Oscore)

# Are they normally distributed ?
# Shapiro-Wilk normality test for Not Used
shapiro.Oscore.notUsed <- with(df.train, shapiro.test(Oscore[Used == "0"]))
# Shapiro-Wilk normality test for Used
shapiro.Oscore.Used <- with(df.train, shapiro.test(Oscore[Used == "1"]))

# Are they identical?
# Mann-Whitney-Wilcoxon test:
wilcox.Oscore <- with(df.train, wilcox.test(Oscore[Used == "0"], Oscore[Used == "1"]))

#     d. Agreeableness ####
# Agreeableness (A-score) plot]
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Ascore))

# Box plot
feature.Ascore <- "Ascore"
title.Ascore <- "Agreeableness"
title.y.Ascore <- "A-score"
plot.box.Ascore <- 
  plot.box.personality(df.train, feature.Ascore, title.Ascore, title.y.Ascore)

# Density plot
plot.density.Ascore <- 
  plot.density.personality(df.train, feature.Ascore, title.Ascore, title.y.Ascore)

# Are they normally distributed ?
# Shapiro-Wilk normality test for Not Used
shapiro.Ascore.notUsed <- with(df.train, shapiro.test(Ascore[Used == "0"]))
# Shapiro-Wilk normality test for Used
shapiro.Ascore.Used <- with(df.train, shapiro.test(Ascore[Used == "1"]))

# Are they identical?
# Student t-test
t_test.Ascore <- 
  with(df.train, t.test(Ascore[Used == "0"], Ascore[Used == "1"], var.equal = FALSE))

#     e. Conscientiousness ####
# Conscientiousness (C-score) plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Cscore))

# Box plot
feature.Cscore <- "Cscore"
title.Cscore <- "Conscientiousness"
title.y.Cscore <- "C-score"
plot.box.Cscore <- 
  plot.box.personality(df.train, feature.Cscore, title.Cscore, title.y.Cscore)

# Density plot
plot.density.Cscore <- 
  plot.density.personality(df.train, feature.Cscore, title.Cscore, title.y.Cscore)

# Are they normally distributed ?
# Shapiro-Wilk normality test for Not Used
shapiro.Cscore.notUsed <- with(df.train, shapiro.test(Cscore[Used == "0"]))
# Shapiro-Wilk normality test for Used
shapiro.Cscore.Used <- with(df.train, shapiro.test(Cscore[Used == "1"]))

# Mann-Whitney-Wilcoxon test:
wilcox.Cscore <- with(df.train, wilcox.test(Cscore[Used == "0"], Cscore[Used == "1"]))

#     f. Impulsivity ####
# Impulsivity plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Impulsive))

# Box plot
feature.Imp <- "Impulsive"
title.Imp <- "Impulsivity"
title.y.Imp <- "Impulsivity"
plot.box.Imp <- 
  plot.box.personality(df.train, feature.Imp, title.Imp, title.y.Imp)

# Density plot
plot.density.Imp <- 
  plot.density.personality(df.train, feature.Imp, title.Imp, title.y.Imp)

# Are they normally distributed ?
# Shapiro-Wilk normality test for Not Used
shapiro.Impulsive.notUsed <- with(df.train, shapiro.test(Impulsive[Used == "0"]))
# Shapiro-Wilk normality test for Used
shapiro.Impulsive.Used <- with(df.train, shapiro.test(Impulsive[Used == "1"]))

# Mann-Whitney-Wilcoxon test:
wilcox.Impulsive <- with(df.train, wilcox.test(Impulsive[Used == "0"], Impulsive[Used == "1"]))
#     g. Sensation-seeking ####
# Sensation-seeking plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(SS))

# Box plot
feature.SS <- "SS"
title.SS <- "Seeking sensations"
title.y.SS <- "Seeking sensations"
plot.box.SS <- 
  plot.box.personality(df.train, feature.SS, title.SS, title.y.SS)

# Density plot
plot.density.SS <- 
  plot.density.personality(df.train, feature.SS, title.SS, title.y.SS)

# plot.density.SS <- df.train %>% 
#   ggplot(aes(x = SS, fill = Used, color = I(color))) +
#   geom_density(alpha = alpha) +
#   labs(title = "Seeking sensations",
#        x = "Sensation-seeking score",
#        y = "") +
#   scale_fill_manual(name = "User?", 
#                     values = c("0" = used_colors[1], "1" = used_colors[2]), 
#                     labels = c("No", "Yes")) +
#   scale_y_continuous(limits = c(0, .5)) +
#   scale_x_continuous(limits = c(-3, 3), breaks = breaks) +
#   geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[1,3]), color = used_colors[1]) +
#   geom_vline(linetype="dashed", xintercept  = as.numeric(mean.score[2,3]), color = used_colors[2]) +
#   theme(axis.text.x = element_text(angle = angle, hjust = 1))

plot.box.SS <- df.train %>% 
  ggplot(aes(y = SS, x = Used, fill = Used, color = I(color))) +
  geom_boxplot()  +
  labs(title = "Seeking sensations",
       y = "Seeking sensations",
       x = "") +
  scale_fill_manual(name = "User?", 
                    values = c("0" = used_colors[1], 
                               "1" = used_colors[2]), 
                    labels = c("No", "Yes")) +
  scale_y_continuous(limits = c(-3, 3), breaks = breaks)+
  scale_x_discrete(labels = c("No", "Yes")) + 
  geom_jitter(width = jitter_width, size = jitter_size)

# Are they normally distributed ?
# Shapiro-Wilk normality test for Not Used
shapiro.SS.notUsed <- with(df.train, shapiro.test(SS[Used == "0"]))
# Shapiro-Wilk normality test for Used
shapiro.SS.Used <- with(df.train, shapiro.test(SS[Used == "1"]))

# Mann-Whitney-Wilcoxon test:
wilcox.SS <- with(df.train, wilcox.test(SS[Used == "0"], SS[Used == "1"]))

#     Summary table for t-tests/Wilcox (feature means) ####
table.indep <- 
  tibble(Feature = c("Neuroticism", "Extraversion (means)", "Extraversion (variances)","Openness to experience", 
                   "Agreeableness", "Conscientiousness", "Impulsivity", 
                   "Sensation-seeking"),
         p.value = c(sprintf("%0.3f", wilcox.Nscore$p.value), 
                     sprintf("%0.3f", t_test.Escore$p.value), sprintf("%0.3f", t_test.var.Escore$p.value),
                     sprintf("%0.3f", wilcox.Oscore$p.value), 
                     sprintf("%0.3f", t_test.Ascore$p.value), sprintf("%0.3f", wilcox.Cscore$p.value), sprintf("%0.3f", wilcox.Impulsive$p.value), 
                     sprintf("%0.3f", wilcox.SS$p.value)),
         User_NonUser = c("Different", "Identical", "Identical", "Different", "Different", 
                        "Different", "Different", "Different"))

# C: Modeling ####
#   Modeling plot parameters ####
imp_text_size <-7

#   1. Pre-processing ####
#     a. Correlation ####
#       Correlation plot utilities ####
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Reorder correlation matrix as a function of distance bw features
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}

corr_plot <- function(df, title) { # *** Main routine ***
  cormat <- round(cor(df, method = 'pearson'), 2)
  #cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  upper_tri
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  # Create the plot
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#998ec3", high = "#f1a340", mid = "#f7f7f7",
                         midpoint = 0., limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1))+
    theme(axis.text.y = element_text(vjust = 0,
                                     size = 12, hjust = 1))+
    coord_fixed() +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.55, 0.725),
      legend.direction = "horizontal") +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  #Return  heatmap
  return(ggheatmap)
  #  return(cormat)
}

#     a. Correlation plot ####
df.cor <- df.train %>% select(Age, Gender, Education, Country, Ethnicity, 
                              Nscore, Escore, Oscore, Ascore, Cscore, 
                              Impulsive, SS)
df.cor <- df.cor-min(df.cor) # Shift values (algorithm requires positive values)
df.cor <- df.cor %>% mutate(Used = as.integer(as.character(df.train$Used)))

chisq <- chisq.test(df.cor, 
                    simulate.p.value = TRUE)
#cormat <- as_tibble(corr_plot(chisq$residuals, "Feature correlation"))
cormat <- round(cor(chisq$residuals, method = 'pearson'), 2)
plot.corr <- corr_plot(chisq$residuals, "Correlation heatmap")

# C: Modeling
#     b. Analysis of low variance ####
nzv <- nearZeroVar(df.train %>% select(-Used))
#     c. RFE ####
#       Wrapper for caret RFE ####

# RFE controls
rfeControl <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 10, # Change to 10 for final run
                      verbose = FALSE)

rfe_drug <- function(df, outcomeName){ 
# Remove the id column
df <- df %>% select(-Id) 

# Make the Used class a factor
df$Used <- factor(df$Used,
                  levels = c(0, 1), 
                  labels = c("0", "1"))

# Exclude Class from the list of predictors
predictors <- names(df)[!names(df) %in% outcomeName]

# caret RFE Call
pred_Profile <- rfe(df[ ,predictors], 
                    unlist(df[ ,outcomeName]), 
                    rfeControl = rfeControl)
return(pred_Profile)
}
#       i. RFE call ####
outcomeName <- "Used"
set.seed(5)
rfe_Profile <- rfe_drug(df.train, outcomeName)
#rfe_Profile
predictors <- predictors(rfe_Profile)
imp <- varImp(rfe_Profile, scale = TRUE)
#       ii. RFE profile plot ####
plot.profile.rfe <- 
  plot(rfe_Profile, type=c("g", "o"), 
       cex = 1.0, 
       col = 1:length(predictors))

#       iii. RFE importance plot ####
imp <- tibble(pred = rownames(imp),  imp)
colnames(imp) <- c("pred", "imp")

plot.importance.rfe <- imp %>% ggplot(aes(reorder(pred, imp$Overall), imp$Overall)) +
  geom_bar(stat = "identity",
           width = .25,
           aes(fill = I(fill),
               color = I(color))) +
  labs(title = "Feature importance from RFE",
       x = "Predictor",
       y ="Importance") +
  coord_flip()
#   2. Training ####
#     Training parameters ####
fitControl <- trainControl( 
  method = "repeatedcv", # Repeated k-fold Cross-Validation
  number = 10, # 5 for debugging CHANGE TO 10-fold CV
  repeats = 10, # 5 for debugging CHANGE TO 10 repeats
  allowParallel = TRUE,
  verbose = FALSE
)
metric <- "Accuracy"
#     Model importance plot utility ####
plot.varImp <- function(model, title){
  varImp <- as.data.frame(varImp(model)$importance)
  varImp <- tibble(rownames(varImp), varImp)
  colnames(varImp) <- c("prediction", "importance")
  plot <- varImp %>% ggplot(aes(reorder(prediction, importance$Overall), varImp$importance$Overall)) +
    geom_bar(stat = "identity",
             width = .25,
             aes(fill = I(fill),
                 color = I(color))) +
    labs(title = title,
         x = "",
         y = "") +
    coord_flip() +
    theme(text = element_text(size=imp_text_size))

}
#     a. Generalized linear model####
set.seed(50)
model.glm <- train(df.train[,predictors], 
                   as.factor(df.train[,outcomeName]), 
                   method = 'glm', 
                   metric = metric)

plot.varImp.glm <- plot.varImp(model.glm, "GLM")

CM.glm <- confusionMatrix(predict(model.glm, newdata = df.test), 
                          df.test$Used)
CM.glm$byClass["Specificity"]
#     b. Generalized linear model with penalized maximum likelihood ####
#       + GLMnet without parameter tuning ####
set.seed(35)
model.glmnet.base <- train(df.train[,predictors],
                           as.factor(df.train[,outcomeName]),
                           method = 'glmnet',
                           metric = metric)

# Plot predictors' relative importance
plot.varImp.glmnet.base <- plot.varImp(model.glmnet.base, "GLMnet (no tuning)")

CM.glmnet.base <- confusionMatrix(predict(model.glmnet.base, newdata = df.test),
                                  df.test$Used)

#       + GLMnet with parameter tuning ####
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
                      metric = metric)

# Plot predictors' relative importance
plot.varImp.glmnet <- plot.varImp(model.glmnet, "GLMnet") 

CM.glmnet <- confusionMatrix(predict(model.glmnet, 
                                     newdata = df.test), 
                             df.test$Used)
#     c. Decision trees ####
#       + RPART without parameter tuning ####
set.seed(90)
model.rpart.base <- train(df.train[,predictors], 
                          as.factor(df.train[,outcomeName]), 
                          method = 'rpart', 
                          metric = metric)

# Plot predictors' relative importance
plot.varImp.rpart.base <- plot.varImp(model.rpart.base, "RPART (no tuning)")

CM.rpart.base <- confusionMatrix(predict(model.rpart.base, newdata = df.test), 
                                 df.test$Used)
#       + RPART with parameter tuning ####
set.seed(80)
model.rpart <- train(df.train[,predictors],
                     as.factor(df.train[,outcomeName]), 
                     method = "rpart",
                     trControl = fitControl,
                     tuneLength = 500, 
                     parms = list(split = 'information'))

# Plot predictors' relative importance
plot.varImp.rpart <- plot.varImp(model.rpart, "RPART")

CM.rpart <- confusionMatrix(predict(model.rpart, newdata = df.test), 
                            df.test$Used)

#     d. Random forest ####
#       + RF without parameter tuning ####
set.seed(25)
model.rf.base <- train(df.train[,predictors], 
                       as.factor(df.train[,outcomeName]), 
                       method = 'rf', 
                       metric = metric)

# Plot predictors' relative importance
plot.varImp.rf.base <- plot.varImp(model.rf.base, "RF (no tuning)")

CM.rf.base <- confusionMatrix(predict(model.rf.base, newdata = df.test), 
                              df.test$Used)
#       + RF with parameter tuning ####
grid.rf <- expand.grid(mtry = seq(1, 10))
set.seed(25)
model.rf <- train(df.train[,predictors], 
                  as.factor(df.train[,outcomeName]),
                  method = 'rf',
                  data = df.train,
                  tuneGrid = grid.rf)

# Plot predictors' relative importance
plot.varImp.rf <- plot.varImp(model.rf, "RF")

CM.rf <- confusionMatrix(predict(model.rf, newdata = df.test), 
                         df.test$Used)

#     e. Stochastic gradient boosting ####
#       + GBM without parameter tuning ####
set.seed(5)
model.gbm.base <- train(df.train[,predictors], 
                        as.factor(df.train[,outcomeName]), 
                        method = 'gbm', 
                        metric = metric)

plot.varImp.gbm.base <- plot.varImp(model.gbm.base, "GBM (no tuning)")

CM.gbm.base <- confusionMatrix(predict(model.gbm.base, newdata = df.test), 
                               df.test$Used)
#       + GBM with parameter tuning ####
max.depth <- floor(sqrt(NCOL(df.train)))
# The grid values below were determined after many iterations
grid.gbm <- expand.grid(n.trees = seq(190, 210, 1),
                        shrinkage = seq(.01, .1, .01),
                        n.minobsinnode = 8,
                        interaction.depth = 3)
set.seed(5)

invisible(capture.output( # Prevent caret::train gbm to print to stdout (trace=FALSE non-op)
  model.gbm <- train(df.train[,predictors],
                     as.factor(df.train[,outcomeName]),
                     method = 'gbm',
                     trControl = fitControl,
                     tuneGrid = grid.gbm) 
))

plot.level.gbm <- plot(model.gbm, plotType = "level")
resampleHist(model.gbm)
plot.varImp.gbm <- plot.varImp(model.gbm, "GBM")
max(model.gbm$results$Accuracy) # Fit to training data

CM.gbm <- confusionMatrix(predict(model.gbm, newdata = df.test), 
                          df.test$Used)
CM.gbm$overall["Accuracy"] # Fit to test data
#     f. Neural network ####
#       + NNET without parameter tuning ####
set.seed(125)
model.nnet.base <- train(df.train[,predictors], 
                         as.factor(df.train[,outcomeName]), 
                         method = 'nnet', 
                         metric = metric)

# Plot predictors' relative importance
plot.varImp.nnet.base <- plot.varImp(model.nnet.base, "NNET (no tuning)")

CM.nnet.base <- confusionMatrix(predict(model.nnet.base, newdata = df.test), 
                                df.test$Used)
#       + NNET with parameter tuning ####
grid.nnet <- expand.grid(size = c(1:6),
                         decay = seq(0.2, 0.3, 0.01))
set.seed(125)
invisible(capture.output( # Prevent caret::train nnet to print to stdout  (trace=FALSE non-op)
  model.nnet <- train(df.train[,predictors],
                      as.factor(df.train[,outcomeName]),
                      method = 'nnet',
                      trControl = fitControl,
                      tuneGrid = grid.nnet,
                      trace = FALSE)
))

# Heat map of the contribution of the size and decay parameters
plot.level.nnet <- plot(model.nnet, plotType = "level")

# Plot predictors' relative importance
plot.varImp.nnet <- plot.varImp(model.nnet, "NNET")

CM.nnet <- confusionMatrix(predict(model.nnet, newdata = df.test), 
                           df.test$Used)
#     g. Model comparisons ####

#       + Fit comparisons ####
model.fit <- tibble(
  method = c("GLM",
             "GLMnet",
             "RPART",
             "RF",
             "GBM",
             "NNET"),
  train = c(max(model.glm$results$Accuracy),
            max(model.glmnet$results$Accuracy),
            max(model.rpart$results$Accuracy),
            max(model.rf$results$Accuracy),
            max(model.gbm$results$Accuracy),
            max(model.nnet$results$Accuracy)),
  test = c(CM.glm$overall[metric],
           CM.glmnet$overall[metric],
           CM.rpart$overall[metric],
           CM.rf$overall[metric],
           CM.gbm$overall[metric],
           CM.nnet$overall[metric]))

naive <- ((df.train %>% filter(Used == 1) %>% nrow()) / nrow(df.train) )

plot.model.fit <- model.fit %>% 
  ggplot(aes(reorder(x = method, test), 
             y = test)) +
  geom_bar(stat = "identity", 
           aes(color = I(color), 
               fill = I(fill))
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(title = "Model comparison",
       x = "",
       y = "Fit to test set") +
  geom_text(aes(label = sprintf("%0.2f%%", test*100), 
                hjust = 1.25),
            size=4,
            color = "white") +
  geom_hline(linetype="dashed", yintercept  = naive, color = "red") +
  geom_text(aes(0, naive, label = "naive", hjust = -1, vjust = -1), angle = 90, color = "red") +
  scale_y_continuous(breaks = seq(0, 1, .1),
                     labels = scales::percent_format(accuracy = 1)) +
  coord_flip()
# Save environment as drugEnvironment.RData####
save.image(file='drugEnvironment.RData')
