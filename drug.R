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
remote <- 0

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
#     Global plot parameters ####
fill <- 'skyblue3'
color <- 'grey'
fill_no <- "#af8dc3"
fill_yes <- "#7fbf7b"
used_colors <- c(fill_no, fill_yes) # No/Yes
alpha <- 0.4 # alpha-blending
axis_text_size <- 10
#   1. Class distribution plot ####
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
#   2. Contingency plots prior to re-binning (balloon plots) ####
#     a. Age contingency plot ####
cont.age <- table(df.train$Age, df.train$Used)
colnames(cont.age) <- c("Not used", "Used")
rownames(cont.age) <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

plot.balloon.age <- balloon.plot(cont.age, "Age group")

#     b. Gender contingency plot ####
cont.gender <- table(df.train$Gender, df.train$Used)
colnames(cont.gender) <- c("Not used", "Used")
rownames(cont.gender) <- c("male", "female")

plot.balloon.gender <- balloon.plot(cont.gender, "Gender")

#     c. Education contingency plot ####
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

#     d. Country contingency plot ####
cont.country <- table(df.train$Country, df.train$Used)
colnames(cont.country) <- c("Not used", "Used")
rownames(cont.country) <- c("USA", "New Zealand", "Other", "Australia", 
                            "Republic of Ireland", "Canada", "UK")

plot.balloon.country <- balloon.plot(cont.country, "Country")

#     e. Ethnicity contingency plot ####
cont.ethn <- table(df.train$Ethnicity, df.train$Used)
colnames(cont.ethn) <- c("Not used", "Used")
rownames(cont.ethn) <- c("Black", "Asian", "White", "Mixed-White/Black", 
                         "Other","Mixed-White/Asian", "Mixed-Black/Asian")
plot.balloon.ethn <- balloon.plot(cont.ethn, "Ethnicity")

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
#   3. Contingency plot utilities ####
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
propPlot <- function(cont, labels, title){
    plot <- cont %>%
      ggplot(aes(x = labels, y = Never_used/Used)) +
      geom_bar(stat = "identity", aes(fill = I(fill_no), color = I(color))) +
      scale_x_discrete(labels = labels)  +
      labs(title = title,
           x = "",
           y = "") +
      scale_y_continuous(breaks = seq(0, 1, .1),
                         labels = scales::percent_format(accuracy = 1)) +
      theme(text = element_text(size = axis_text_size))
    return(plot)
  }

#   4. Contingency plots ####
#     a. Age  ####
title.age <- "Age"
labels.age <- c("18-24", "25-34", "35-44", "45-54", "55+")
plot.age <- demogPlot(title.age, labels.age, "years old")

#     b. Gender ####
title.gender <- "Gender"
labels.gender <- c("male", "female")
plot.gender <- demogPlot(title.gender, labels.gender, "")

#     c. Education ####
title.edu <- "Education"
labels.edu <- c("Left school as teen", 
                "Some college/univ.", 
                "Prof. certif./diploma",
                "Univ. degree", 
                "Graduate degree")
plot.edu <- demogPlot(title.edu, labels.edu, "")

#     d. Country ####
title.country <- "Country"
labels.country <- c("USA", "Other", "UK")
plot.country <- demogPlot(title.country, labels.country, "")

#     e. Ethnicity ####
title.ethn <- "Ethnicity"
labels.ethn <- c("White", "Non-white")
plot.ethn <- demogPlot(title.ethn, labels.ethn, "")

#     f. Combine the 5 contingency plots ####
plot.contingency <- grid.arrange(plot.country, plot.gender, plot.ethn,
             plot.age, plot.edu,
             layout_matrix = rbind(c(1, 1, 2, 2, 3, 3), 
                                   c(4, 4, 4, 5, 5, 5)),
             top = "Use of cannabis in training set by:",
             left = "Counts")

#   5. Personality analysis ####
#     Personality analysis plot parameters ####
breaks <- seq(-3, 3, .5)
angle <- 60
#     a. Neuroticism ####
# Neuroticism (N-score) plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Nscore))

plot.density.Nscore <- df.train %>% 
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
#     b. Extraversion ####
# Extraversion (E-score) plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Escore))

plot.density.Escore <- df.train %>% 
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
#     c. Openness to experience ####
# Openness to experience (O-score) plot 
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Oscore))

plot.density.Oscore <- df.train %>% 
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
#     d. Agreeableness ####
# Agreeableness (A-score) plot]
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Ascore))

plot.density.Ascore <- df.train %>% 
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
#     e. Conscientiousness ####
# Conscientiousness (C-score) plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Cscore))

plot.density.Cscore <- df.train %>% 
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
#     f. Impulsivity ####
# Impulsivity plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(Impulsive))

plot.density.Imp <- df.train %>% 
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
#     g. Sensation-seeking ####
# Sensation-seeking plot
mean.score <- df.train %>% 
  group_by(Used) %>% 
  dplyr::summarize(count = n(), mean = mean(SS))

plot.density.SS <- df.train %>% 
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
#     h. Personality plot ####
plot.density.personality <- 
  grid.arrange(plot.density.Nscore, plot.density.Escore, plot.density.Oscore,
             plot.density.Ascore, plot.density.Cscore, plot.density.Imp, plot.density.SS,
             layout_matrix = rbind(c(1, 1, 2, 2, 3, 3), 
                                   c(4, 4, 5, 5, 6, 6), 
                                   c(7, 7, 7, NA, NA, NA)),
             top = "Personality test score distribution",
             left = "Density")
#   6. Analysis of correlation ####
#     Correlation plot utilities ####
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

#     Correlation plot ####
df.cor <- df.train %>% select(Age, Gender, Education, Country, Ethnicity, 
                              Nscore, Escore, Oscore, Ascore, Cscore, 
                              Impulsive, SS)
df.cor <- df.cor-min(df.cor) # Shift values (algorithm requires positive values)
df.cor <- df.cor %>% mutate(Used = as.integer(as.character(df.train$Used)))

chisq <- chisq.test(df.cor, 
                    simulate.p.value = TRUE)
#cormat <- as_tibble(corr_plot(chisq$residuals, "Feature correlation"))
plot.corr <- corr_plot(chisq$residuals, "Feature correlation")

# C: Modeling
#   Modeling plot parameters ####
imp_text_size <-7

#   1. RFE ####
#     Wrapper for caret RFE ####
rfe_drug <- function(df, outcomeName){ 
# Remove the id column
df <- df %>% select(-Id) 

# Make the Used class a factor
df$Used <- factor(df$Used,
                  levels = c(0, 1), 
                  labels = c("0", "1"))

# RFE controls
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3, # Change to 10 for final run
                      verbose = FALSE)
# Exclude Class from the list of predictors
predictors <- names(df)[!names(df) %in% outcomeName]

# caret RFE Call
pred_Profile <- rfe(df[ ,predictors], 
                    unlist(df[ ,outcomeName]), 
                    rfeControl = control)
return(pred_Profile)
}
#     a. RFE call ####
outcomeName <- "Used"
set.seed(5)
rfe_Profile <- rfe_drug(df.train, outcomeName)
#rfe_Profile
predictors <- predictors(rfe_Profile)
imp <- varImp(rfe_Profile, scale = TRUE)
#     b. RFE profile plot ####
plot.profile.rfe <- plot(rfe_Profile, type=c("g", "o"), cex = 1.0, col = 1:length(predictors))

#     c. RFE importance plot ####
imp <- tibble(pred = rownames(imp),  imp)
colnames(imp) <- c("pred", "imp")

plot.importance.rfe <- imp %>% ggplot(aes(reorder(pred, imp$Overall), imp$Overall)) +
  geom_bar(stat = "identity",
           width = .25,
           aes(fill = I(fill),
               color = I(color))) +
  labs(title = "Predictor importance from RFE",
       x = "Predictor",
       y ="Importance") +
  coord_flip()
#   2. Training ####
#     Training parameters ####
fitControl <- trainControl( 
  method = "repeatedcv", # Repeated k-fold Cross-Validation
  number = 3, # 5 for debugging CHANGE TO 10-fold CV
  repeats = 3, # 5 for debugging CHANGE TO 10 repeats
  allowParallel = TRUE,
  verbose = FALSE
)
metric <- "Accuracy"
#     a. Generalized linear model####
set.seed(50)
model.glm <- train(df.train[,predictors], 
                   as.factor(df.train[,outcomeName]), 
                   method = 'glm', 
                   metric = metric)
# Plot predictors' relative importance
varImpPlot.glm <- plot(varImp(object = model.glm), 
                       main = "GLM", 
                       top = length(predictors))

CM.glm <- confusionMatrix(predict(model.glm, newdata = df.test), 
                          df.test$Used)
#     b. Generalized linear model with penalized maximum likelihood ####
#       + GLMnet without parameter tuning ####
set.seed(35)
model.glmnet.base <- train(df.train[,predictors],
                           as.factor(df.train[,outcomeName]),
                           method = 'glmnet',
                           metric = 'Accuracy')

# Plot predictors' relative importance
varImpPlot.glmnet.base <- plot(varImp(object = model.glmnet.base),
                               main = "GLMnet",
                               top = length(predictors)) +
  theme(text = element_text(size=imp_text_size))

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
                      metric = "Accuracy")

# Plot predictors' relative importance
varImpPlot.glmnet <- 
  plot(varImp(object = model.glmnet), 
       main = "GLMnet", 
       top = length(predictors)) +
  theme(text = element_text(size=imp_text_size))

CM.glmnet <- confusionMatrix(predict(model.glmnet, 
                                     newdata = df.test), 
                             df.test$Used)
# Save environment as drugEnvironment.RData####
save.image(file='drugEnvironment.RData')