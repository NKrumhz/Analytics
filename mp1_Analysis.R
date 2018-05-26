# Nathan Krumholz 
#####
# 1/28/17

# Miniproject 1: Spotify playlist analysis 


# libraries and packages 
#library(devtools)
#install_github("tiagomendesdantas/Rspotify")
library(tidyverse)
library(class)
library(e1071)
library(caret)


setwd("F://ME//Analytics//Miniproject 1")

# load data 
{
    dinner <- read.csv("dinner_track.csv")   
    party <- read.csv("party_track.csv")
    sleep <- read.csv("sleep_track.csv")
    workout <- read.csv("workout_track.csv")

    dinner$label <- rep("dinner", nrow(dinner))
    party$label <- rep("party", nrow(party))
    sleep$label <- rep("sleep", nrow(sleep))
    workout$label <- rep("workout", nrow(workout))


    A.dat <- rbind(dinner, party)
    A.dat <- rbind(A.dat, sleep)
    A.dat <- rbind(A.dat, workout)
}
# removing NA's 
A.dat <- A.dat[-(992),]
# remove duplicates (only need to compare the song id) 
#A.dat <- A.dat[!duplicated(A.dat[,1]),]
    

# inital analysis 
A.dat$label <- as.factor(A.dat$label)

A.dat$key <- as.factor(A.dat$key)
A.dat$time_signature <- as.factor(A.dat$time_signature)

A.dat$id <- A.dat$name <- A.dat$uri <- A.dat$artist <- NULL # removed becuase they are non-reoccuring factors 
A.dat$time_signature <- A.dat$mode <- NULL                  # removed becuase they were determined to have low importance 
summary(A.dat)

# duration_ms, liveness are heavily skewed left 
#normalizing other data points 
normaliz <- function(value) {
    (value - min(value)) / (max(value) - min(value))
}

A.dat$duration_ms <- normaliz(log10(A.dat$duration_ms)+1)
A.dat$liveness <- normaliz(log10(A.dat$liveness)+1)
A.dat$speechiness <- normaliz(log10(A.dat$speechiness)+1)


pairs(~acousticness + danceability + duration_ms + instrumentalness + liveness + loudness + speechiness + valence, data = A.dat)
gatherpairs <- function(data, ..., xkey = '.xkey', xvalue = '.xvalue', ykey = '.ykey', yvalue = '.yvalue', na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
    vars <- quos(...)
    xkey <- enquo(xkey)
    xvalue <- enquo(xvalue)
    ykey <- enquo(ykey)
    yvalue <- enquo(yvalue)

    data %>% {
        cbind(gather(., key = !!xkey, value = !!xvalue, !!!vars, na.rm = na.rm, convert = convert, factor_key = factor_key), dplyr::select(., !!!vars))
    }   %>% gather(., key = !!ykey, value = !!yvalue, !!!vars, na.rm = na.rm, convert = convert, factor_key = factor_key)
}

A.dat %>% gatherpairs(acousticness, danceability, duration_ms, energy, instrumentalness, liveness, loudness, speechiness, valence) %>%
    ggplot(aes(x = .xvalue, y = .yvalue, color = label), alpha = 0.2) +
    geom_point() +
    facet_wrap(.xkey ~ .ykey, scales = 'free')



#####
# making testing and training data 
{
test <- sample(1:nrow(A.dat), 0.2*nrow(A.dat))
train <- setdiff(1:nrow(A.dat), test)

known <- A.dat[test, 12]
}
#####
# model fitting 
#####

### logistic regression 

A.dat$dinner <- as.logical(ifelse(A.dat$label == "dinner", T, F))
A.dat$sleep <- as.logical(ifelse(A.dat$label == "sleep", T, F))
A.dat$party <- as.logical(ifelse(A.dat$label == "party", T, F))
A.dat$workout <- as.logical(ifelse(A.dat$label == "workout", T, F))
A.dat$label <- NULL

fit.d <- glm(dinner ~ ., data = A.dat[train, c(1:11,12)], family = binomial)
fit.s <- glm(sleep ~ ., data = A.dat[train, c(1:11, 13)], family = binomial)
fit.p <- glm(party ~ ., data = A.dat[train, c(1:11, 14)], family = binomial)
fit.w <- glm(workout ~ ., data = A.dat[train, c(1:11, 15)], family = binomial)

pred.d <- predict(fit.d, A.dat[test, 1:11], type = "response")
pred.s <- predict(fit.s, A.dat[test, 1:11], type = "response")
pred.p <- predict(fit.p, A.dat[test, 1:11], type = "response")
pred.w <- predict(fit.w, A.dat[test, 1:11], type = "response")

p.preds <- data.frame(dinner = pred.d, sleep = pred.s, party = pred.p, workout = pred.w)
sults <- as.factor(colnames(p.preds)[apply(p.preds,1,which.max)])
print(confusionMatrix(data = sults, reference = known))

    # Accuracy: 28.6%

### fit random forest 
library(randomForest)
rfst <- randomForest(label ~ ., ntree = 100, data = A.dat[train,])


plot(rfst)
print(rfst)
varImpPlot(rfst)
varImp(rfst)
    pred <- predict(rfst, A.dat[test, 1:11])

print(confusionMatrix(data = pred, reference = known))
    # Accuracy = 75.5
    # Kappa = 0.668

    # variable importance 
varImp(rfst)

### fit svm 
uea1 <- svm(label ~ ., data = A.dat[train,])

    pred <- predict(uea1, A.dat[test, 1:11])
print(confusionMatrix(data = pred, reference = known))
    # Accuracy = 76.2
    # Kappa = 0.678

### fit gbm 
library(gbm)

fbm <- gbm(label ~ ., data = A.dat[train,], shrinkage = 0.005, n.trees = 5000, distribution = 'multinomial', cv.folds = 10)
biter = gbm.perf(fbm, method = "cv")

val <- predict(fbm, A.dat[test, 1:11], n.trees = biter, type = 'response')
pred <- apply(val, 1, which.max)
pred <- factor(pred, levels = c(1:4), labels = c("dinner", "party", "sleep", "workout"))
print(confusionMatrix(data = pred, reference = known))
    # Accuracy = 75.9
# Kappa = 0.673

