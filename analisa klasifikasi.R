#Read Data
wholesale <- read.csv("C:/Users/asus/Documents/UAS Data Mining/Wholesale.csv")
head(wholesale)
library(tidyverse)
glimpse(wholesale)

#Data Wrangling
wholesale <- wholesale %>% 
  select(-Region) %>% 
  mutate(Channel = factor(Channel, levels = c(1, 2), labels = c("Horeca", "Retail")))
wholesale

#Analisis Data Explorasi
prop.table(table(wholesale$Channel))
summary(wholesale)

#REGRESI LOGISTIK
#Split Data
RNGkind(sample.kind = "Rounding")
set.seed(123)
intrain <- sample(nrow(wholesale), nrow(wholesale)*0.8)
wholesale.train <- wholesale[intrain, ]
wholesale.test <- wholesale[-intrain, ]
prop.table(table(wholesale.train$Channel))
#Pemodelan
model_log <- glm(Channel ~ ., data = wholesale.train, family = "binomial")
summary(model_log)
backward <- step(model_log, direction = "backward")
summary(backward)
#prediksi
summary(backward)
wholesale.test$log.Risk <- predict(backward, newdata = wholesale.test, type = "response")
ggplot(wholesale.test, aes(x=log.Risk)) +
  geom_density(lwd=0.5) +
  labs(title = "Distribusi Peluang Data Prediksi") +
  theme_light()
wholesale.test$log.Label <- factor(ifelse(wholesale.test$log.Risk > 0.5, "Retail","Horeca"))
wholesale.test[1:10, c("log.Label", "Channel")]

#KNN
#Split Data
wholesale_train_x <- wholesale.train[,-1]

wholesale_test_x <- wholesale.test[,-c(1,8,9)]

wholesale_train_y <- wholesale.train[,1]

wholesale_test_y <- wholesale.test[,1]
#Scaling
wholesale_train_xs <- scale(x = wholesale_train_x)
wholesale_test_xs <- scale(x = wholesale_test_x, 
                           center = attr(wholesale_train_xs, "scaled:center"), 
                           scale = attr(wholesale_train_xs, "scaled:scale"))
#klasifikasi
round(sqrt(nrow(wholesale_train_xs)))
library(class)

knn.Label <- knn(train = wholesale_train_xs, 
                 test = wholesale_test_xs,
                 cl = wholesale_train_y,
                 k = 19)
head(knn.Label)

#EVALUASI MODEL
#Regresi Logistik
library(caret)
cm_log <- confusionMatrix(data = wholesale.test$log.Label, 
                          reference = wholesale.test$Channel)
cm_log
eval_logit <- tibble(Accuracy = cm_log$overall[1],
                     Recall = cm_log$byClass[1],
                     Specificity = cm_log$byClass[2],
                     Precision = cm_log$byClass[3])
eval_logit
#Tuning cutoff
performa <- function(cutoff, prob, ref, postarget, negtarget) 
{
  predict <- factor(ifelse(prob >= cutoff, postarget, negtarget))
  conf <- confusionMatrix(predict , ref, positive = postarget)
  acc <- conf$overall[1]
  rec <- conf$byClass[1]
  prec <- conf$byClass[3]
  spec <- conf$byClass[2]
  mat <- t(as.matrix(c(rec , acc , prec, spec))) 
  colnames(mat) <- c("recall", "accuracy", "precicion", "specificity")
  return(mat)
}

co <- seq(0.01,0.80,length=100)
result <- matrix(0,100,4)

for(i in 1:100){
  result[i,] = performa(cutoff = co[i], 
                        prob = wholesale.test$log.Risk, 
                        ref = wholesale.test$Channel, 
                        postarget = "Horeca", 
                        negtarget = "Retail")
}

tibble("Recall" = result[,1],
       "Accuracy" = result[,2],
       "Precision" = result[,3],
       "Specificity" = result[,4],
       "Cutoff" = co) %>% 
  gather(key = "performa", value = "value", 1:4) %>% 
  ggplot(aes(x = Cutoff, y = value, col = performa)) +
  geom_line(lwd = 1.5) +
  scale_color_manual(values = c("darkred","darkgreen","orange", "blue")) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "Tradeoff model perfomance") +
  theme_light() +
  theme(legend.position = "top",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())
#KNN
cm_knn <- confusionMatrix(data = knn.Label, 
                          reference = wholesale_test_y)
cm_knn
eval_knn <- tibble(Accuracy = cm_knn$overall[1],
                   Recall = cm_knn$byClass[1],
                   Specificity = cm_knn$byClass[2],
                   Precision = cm_knn$byClass[3])
eval_knn