## install those packages 
## x <- c('tidyverse', 'caret', 'klaR', 'e1071')
## install.packages(x)
## rm(x)

##  Prediction task : determine whether a person makes over 50  a year.

library(tidyverse)
## Import & Tidy --------
adult = read_csv("adult/adult.data", col_names = FALSE, na = '?')

## set nama kolom
adult_headers = c(
  'age', 'workclass', 'fnlwgt', 'education', 'education_num', 
  'marital', 'occupation', 'relationship', 'race', 'sex', 
  'capital_gain', 'capital_loss', 'hoursperweek', 'native_country', 'y'
)

adult = adult %>% 
  magrittr::set_colnames(adult_headers) %>% 
  mutate(id = row_number()) %>% 
  select(id, y, everything())

View(adult)

## pilih variabel tertentu 
adult = adult %>% 
  select(
    id, y, 
    age, education, education_num, 
    race, sex, hoursperweek, native_country
  )

## Feature engineering ------
## faktorisasi 
adult = adult %>% 
  mutate(
    y = y %>% as.factor(), 
    #workclass = workclass %>% as.factor(), 
    education = education %>% as.factor(), 
    #marital = marital %>% as.factor(), 
    #occupation = occupation %>% as.factor(), 
    #relationship = relationship %>% as.factor(),
    race = race %>% as.factor(), 
    sex = sex %>% as.factor(), 
    native_country = native_country %>% as.factor()
  )

## penyederhanaan kategori 
adult = adult %>% 
  mutate(
    y = if_else(y == '>50K', 'more50', "equal_less50") %>% as.factor(),
    race2 = if_else(race == "White", "White", "Other") %>% as.factor(),
    native = if_else(native_country == "United-States", "yes", "no", missing = "yes") %>% as.factor()
  )

education.lookup = adult %>% 
  select(education, education_num) %>% 
  group_by(education, education_num) %>% 
  summarise(n  = n()) %>% 
  arrange(education_num) %>% 
  mutate(
    edu = case_when(
      education_num <= 8                        ~ 'UHS', ## Under High-School 
      education_num == 9                        ~ 'HS', ## High School Graduated
      education_num >= 10 & education_num <= 13 ~ "Bachelors", ## Barchelors 
      education_num >= 14                       ~ "PostGrad" ## Post-Graduated
    )
  )
View(education.lookup)

adult = adult %>% 
  left_join(
    education.lookup %>% select(-n), 
    by = c("education", "education_num")
  ) %>% 
  mutate(
    edu = edu %>% as.factor()
  )

## Visualization ----------------
theme_set(theme_bw())

## kebanyakan penduduk umurnya dibawah 50 tahun 
## penduduk umur usia produktif
adult %>% 
  ggplot(aes(x = age)) + 
  geom_histogram()

adult %>% 
  ggplot(aes(x = age, fill = y)) + 
  geom_histogram()

## tingkatan pendidikan 
adult %>% 
  ggplot(aes(x = age, fill = y)) + 
  geom_histogram() + 
  facet_wrap(~edu)

## pengaruh jenis kelamin
adult %>% 
  ggplot(aes(age)) + 
  geom_histogram(aes(fill = y)) + 
  facet_wrap(~ sex)

## pengaruh ras 
adult %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(aes(fill = y)) + 
  facet_wrap(~ race2)

## pengaruh ras dan native_us
adult %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(aes(fill = y)) + 
  facet_wrap(~ race2 + native)

## Slice / Split data ----------
library(caret)

adult = adult %>% 
  dplyr::select(-education, -education_num, -race, -native_country)

## Membuat 70 / 30 untuk training data. 
set.seed(9999)
indexes = createDataPartition(y = adult$y, 
                              times = 1,
                              p = 0.7,
                              list = FALSE)

adult.train = adult[indexes,]
adult.test = adult[-indexes,]

## persentase pada y relatif tidak berubah 
prop.table(table(adult$y))
prop.table(table(adult.train$y))
prop.table(table(adult.test$y))

## 10-fold Cross Validation.. 
train_ctrl = trainControl(method = "cv", 
                          number = 10, 
                          savePredictions = TRUE, 
                          classProbs = TRUE)

## Modelling --------------------------------
formula = y ~ age + edu + native + race2 + sex

## Decision Tree 
set.seed(3333)
tree.train.1 = train(
  formula,
  data = adult.train, 
  method = 'rpart', 
  parms = list(split = "information"),
  trControl=train_ctrl,
  tuneLength = 10
)
tree.pred.1 = predict(tree.train.1, newdata = adult.test)
cm.tree = confusionMatrix(tree.pred.1, reference = adult.test$y, positive = 'more50')

## Naive Bayesian
set.seed(6666)
nb.train.1 = train(
  formula,
  data = adult.train, 
  method = 'nb',
  trControl=train_ctrl,
  tuneLength = 10
)

## beberapa observasi di Naive Bayesian memiliki peluang = 0
warnings()

nb.pred.1 = predict(nb.train.1, newdata = adult.test)
cm.nb = confusionMatrix(nb.pred.1, reference = adult.test$y, positive = 'more50')

## Model Checking ------------------

##  Gambar Pohon Keputusan -- Bacanya agak susah !! 
rpart.plot::prp(tree.train.1$finalModel, box.palette = "BuGn", tweak = 1.2)

## Importance of variables 
varImp(tree.train.1) %>% plot()

## Confusion Matrix
cm.tree
cm.nb

## ROC 
library(plotROC)
library(mlbench)
library(pROC)

### ROC -- Decision Tree
tree.roc = roc(adult.test$y %>% as.numeric(), 
               tree.pred.1 %>% as.numeric()
               )
plot(tree.roc, print.thres="best", print.thres.best.method="closest.topleft")
auc(tree.roc) 

### ROC -- Naive Bayes
nb.roc = roc(adult.test$y %>% as.numeric(), 
                  nb.pred.1 %>% as.numeric()
                  )
plot(nb.roc, print.thres="best", print.thres.best.method="closest.topleft")
auc(nb.roc)

## Kesimpulan --------- 
## 1. Algoritma pengklasifikasi yang terbaik bagi data Adult 
## dengan variabel yang telah dipilih adalah 'Decision Tree'.
## 2. Decision Tree memiliki Sensitivity dan AUC-ROC, yang lebih tinggi 
## Sensitivity : (proporsi prediksi observasi yang memiliki income >50K yang pada aktualnya observasi tersebut memang memiliki income >50K)
## TuningLength berpengaruh kepada performa dari Decision Tree, namun 
## tidak berpengaruh pada Naive Bayes.