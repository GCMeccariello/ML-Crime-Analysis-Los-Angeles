setwd('/Users/gcm/Library/Mobile Documents/com~apple~CloudDocs/HSLU/Master/2. Semester/MPM_AppliedMachineLearning/Group Work/')

# install.packages("dplyr")
# install.packages("reclin")
# install.packages("plyr")
# install.packages("strip")

library(stringr)
library(dplyr)
library(reclin)
library(plyr)
library(strip)

################################################ reading and cleaning ################################################ 

# reading data
data = read.csv('songs_normalize.csv', header=TRUE, stringsAsFactors = TRUE)
data <- data[data['year'] != 2020 & data['year'] != 1998,]  #excluding year 2020, because it contains only 3 observations. and 1998 only 1 observation.
# adding unique identifier
data$ID <- seq.int(nrow(data))


s <- setNames(strsplit(as.character(data$genre), ", "), data$ID)
all_lev <- sort(unique(unlist(s)))

m <- t(sapply(s, function(x) table(factor(x, lev = all_lev))))
m = as.data.frame(m)
m$ID <- seq.int(nrow(m))
m

# merging data with genres
data <- merge(x = data, y = m, by = "ID", all = TRUE)
# taking column "set()" out.
data <- data[!names(data) %in% c("set()")]

head(data)

poisson.data <- data
binomial.data <- data
gam.data <- data
nn.data <- data 

################################################ Linear Model ################################################ 

data.lm <- lm(data$popularity ~ data$duration_ms + data$danceability + data$speechiness + data$acousticness + data$instrumentalness + data$liveness + data$valence + data$tempo +
              data$blues + data$classical + data$country + data$`Dance/Electronic` + data$`easy listening` + data$`Folk/Acoustic` + data$`hip hop` + data$jazz + data$latin + 
                data$metal + data$pop + data$`R&B` + data$rock)
              
f.empty <- lm(data$popularity ~ NULL, data=data)
add1(f.empty, scope=data.lm)

f.1 <- update(f.empty, .~.+data$`Dance/Electronic`)
add1(f.1, scope=data.lm)

f.1 <- update(f.empty, .~.+data$`Dance/Electronic` + data$`hip hop`)
add1(f.1, scope=data.lm)

f.1 <- update(f.empty, .~.+data$`Dance/Electronic` + data$`hip hop` + data$metal)
add1(f.1, scope=data.lm)

f.1 <- update(f.empty, .~.+data$`Dance/Electronic` + data$`hip hop` + data$metal + data$`R&B`)
add1(f.1, scope=data.lm)

f.1 <- update(f.empty, .~.+data$`Dance/Electronic` + data$`hip hop` + data$metal + data$`R&B` + data$duration_ms)
add1(f.1, scope=data.lm)


summary(f.1)

summary(data.lm)


lm.2 <- lm(data$year ~ data$popularity + data$duration_ms + data$danceability + data$speechiness + data$acousticness + data$instrumentalness + data$liveness + data$valence + data$tempo +
             data$blues + data$classical + data$country + data$`Dance/Electronic` + data$`easy listening` + data$`Folk/Acoustic` + data$`hip hop` + data$jazz + data$latin + 
             data$metal + data$pop + data$`R&B` + data$rock, data=data) 
summary(lm.2)

f.empty <- lm(data$year ~ NULL, data=data)
add1(f.empty, scope=lm.2)

f.2 <- update(f.empty, .~. + data$duration_ms, data=data)
add1(f.2, scope=lm.2)

f.2 <- update(f.empty, .~. + data$duration_ms + data$valence, data=data)
add1(f.2, scope=lm.2)

f.2 <- update(f.empty, .~. + data$duration_ms + data$valence + data$`Dance/Electronic`, data=data)
add1(f.2, scope=lm.2)

conf.2 <- confint(lm.2)
##
## 2) plot estimates
par(mar = c(4,5,2,2))
plot(y = 1:4,
     x = rev(coef(f.2)),
     xlim = c(-9, 7),
     xlab = "Estimated coefficients",
     ylab = "",
     axes = FALSE)
box()
axis(side = 2, at = 1:4,
     labels = rev(names(coef(f.2))), 
     las = 2)
axis(side = 1)
##
## 3) plot CIs
segments(x0 = rev(conf.2[, "2.5 %"]),
         x1 = rev(conf.2[, "97.5 %"]),
         y0 = 1:4,
         y1 = 1:4)
abline(v = 0, lty = "dashed")


lm.2 <- lm(data$year ~ data$duration_ms + data$valence + data$danceability + data$instrumentalness)
summary(lm.2)

lm.3 <- lm(data$year, .~. + data$duration_ms, data=data)

add1(lm.3, scope=lm.2)

data.lm <- drop1(data.lm, test='F')
data.lm
data.lm <- drop1(data.lm, test='F')
data.lm
summary(data.lm)


# some plots

boxplot(data$popularity)
boxplot(data$danceability, data$energy, data$speechiness, data$acousticness, data$instrumentalness, data$liveness, data$valence)

plot(data$popularity ~ data$energy + data$danceability)

boxplot(data$duration_ms ~ data$year)


plot(data$popularity ~ data$year )


feature <- noquote(i)
print(feature)
plot(data$popularity ~ explicit, data = data)


plot(data$popularity ~ data$duration_ms, data = data)


df2 <- data[!names(data) %in% c("artist", "song", "explicit", "mode")]
df2 <- df2[sample(nrow(df2), 50), ]
plot(df2, upper.panel = panel.smooth)




# pop, Dance/Electronic,hip hop, R&B,country, latin, 	World/Traditional, rock, 	Folk/Acoustic, metal

# s <- setNames( strsplit(df2$genre, ","), df2$ID )
# s <- c("pop", "Dance/Electronic","hip hop", "R&B","country", "latin", 	"World/Traditional", "rock", 	"Folk/Acoustic", "metal")
s <- setNames( strsplit(as.character(data$genre), ","), data$ID)
all_lev <- sort(unique(unlist(s)))
all_lev
m <- t(sapply(s, function(x) table(factor(x, lev = all_lev))))
m



plot(data$popularity ~ artist, data = data)
colnames(data)

################################################ Generalised Linear Model with family set to Poisson ################################################ 

str(poisson.data)

poisson.data <- poisson.data[!names(poisson.data) %in% c("ID", "artist", "song", "genre", "explicit")]
max <- apply(poisson.data, 2, max)
min <- apply(poisson.data, 2, min)


data_scaled <- as.data.frame(scale(poisson.data, center = min, scale = max - min))
head(data_scaled)

str(data_scaled)
glm.fits <- glm(year ~ ., data=data_scaled, family = poisson)

summary(glm.fits)

################################################ Generalised Linear Model with family set to Binomial ################################################ 

str(binomial.data)

binomial.data <- binomial.data[!names(binomial.data) %in% c("ID", "artist", "song", "genre", "explicit")]
max <- apply(binomial.data, 2, max)
min <- apply(binomial.data, 2, min)


data_scaled <- as.data.frame(scale(binomial.data, center = min, scale = max - min))
head(data_scaled)

str(data_scaled)
glm.fits <- glm(year ~ ., data=data_scaled, family = binomial)

summary(glm.fits)

################################################ Generalised Additive Model ################################################ 

str(gam.data)

head(gam.data)


library(mgcv)
install.packages('gam')
library(gam)

cor(gam.data)
help(cor)

################################################ Neutral Network ################################################ 


library(tidyverse)
library(caret)
library(neuralnet)
library(dplyr)
library(lattice)

head(nn.data)
nn.data <- nn.data[!names(nn.data) %in% c("ID", "artist", "song", "genre", "explicit")]

str(nn.data)

set.seed(014)
indices <- createDataPartition(nn.data$popularity, p = 0.8, list = FALSE)
train <- nn.data %>% slice(indices)
test <- nn.data %>% slice(-indices)
boxplot(train$popularity, test$popularity, nn.data %>% sample_frac(0.2) %>% pull(popularity))

# scaling data 0..1

max <- apply(nn.data, 2, max)
min <- apply(nn.data, 2, min)


data_scaled <- as.data.frame(scale(nn.data, center = min, scale = max - min))

train_scaled <- data_scaled %>% slice(indices)
test_scaled <- data_scaled %>% slice(-indices)


set.seed(015)
data_net = neuralnet(popularity ~ duration_ms + danceability + energy + key + loudness + mode + speechiness + acousticness +instrumentalness +liveness + valence+tempo, train_scaled, hidden = 6 , linear.output = TRUE)
data_net = neuralnet(popularity ~ `Dance/Electronic` + `hip hop` + metal + `R&B` + duration_ms, train_scaled, hidden = 6 , linear.output = TRUE)

plot(data_net)


data_net = neuralnet(popularity ~  duration_ms + danceability + year + speechiness, train_scaled, hidden = 15 , linear.output = FALSE)


# prediction

pred_scaled <- compute(data_net, test_scaled %>% select(-popularity))
pred <- pred_scaled$net.result * (max(nn.data$popularity) - min(nn.data$popularity)) + min(nn.data$popularity)
pred


plot(test$popularity, pred, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating", ylim=c(0,80))
abline(0,1)

# error
sqrt(mean((test$popularity - pred)^2))

################################################ Support Vector Machine ################################################ 




################################################ solve an optimisation problem ################################################ 



