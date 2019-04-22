# Categorical data
setwd("D:/OneDrive/education/R/Statistic_in_R_part1")
library(ggplot2)

# 
df_shops <- read.csv("shops.csv")
df_therapy_data <- read.csv("therapy_data.csv")

# Оценка ~ Задание+Error(Испытуемый/Задание)
# Но так как в разных группах разные испытуемые, то если мы хотим учесть группу,
# то аналогичную операцию не надо делать.
# Оценка ~ Задание+Группа+Error(Испытуемый/Задание) # если задания одинаковые для групп.
# Оценка ~ Задание*Группа+Error(Испытуемый/Задание) # если задания разные для групп.
# Например, заданием является оценить головную боль при приеме препарата у которого 
# головная боль является побочным действием, и препарата против головной боли. 
# То есть предполагается что есть скрытая переменная.
# Оценка ~ (A:1)*Группа+Error(Испытуемый/(A:1))  # заменил задание на A - препарат вызывающий головную боль, 
# B - препарат от головной боли


### ANOVA

library(ggplot2)

# DV - dependent variable
# IV - independent variable

# formulae
# одна переменная позволяет предсказывать другую
DV ~ IV # One-way
# на зависимую переменную влияют 2 независимые переменные
DV ~ IV1 + IV2 # Two-way
# влияние доной пенеменной на другую зависит от уровня третьей переменной
DV ~ IV1:IV2  # Two-way interaction
# формула с главными эффектами  + взаимодействие
DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interaction
# формула с главными эффектами  + взаимодействие
DV ~ IV1 * IV2  # The same: Main effects + interaction
# 
DV ~ IV1 + IV2 + IV3 + IV1:IV2
# 3 независимых переменных (предиктора)
# инетесуют ВСЕ основные эффекты  + взаимодействие до второго уровня
DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interactions up to level 2
# 
DV ~ IV1 + Error(subject/IV1) # repeated measures

# Укажите формулы, которые расшифровываются одинаково

# DV ~ (IV1 + IV2)^2
# DV ~ IV1 * IV2
# DV ~ (IV1*IV2)^2
# -- DV ~ IV1:IV2 + Error(subject/(IV1:IV2))

# здесь немного про формулы с примерами: http://science.nature.nps.gov/im/datamgmt/statistics/r/formulas/
# Вот такой шпаргалки не хватает, может только чуть больше примеров: http://gyazo.com/12f1ee99eeea5ec53c0b1e7dabe7de42�
# ?formula
# https://www.statmethods.net/stats/anova.html


# reading data

mydata <- read.csv('shops.csv')


# One-way ANOVA

boxplot(price ~ origin, data=mydata)

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()


# aov -  запустить дисперсионный анализ и сравниь 2 группы
# DV - price
# IV - origin
fit <- aov(price ~ origin, data=mydata)
summary(fit)


# Two-way ANOVA
# DV - price
# IV - source + store (type of store)
fit1 <- aov(price ~ origin + store, data=mydata)
summary(fit1)

model.tables(fit1,"means")


# Interaction

pd = position_dodge(0.2)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

# DV - price
# IV - source + store (type of store) (interraction for origin:store)
fit3 <- aov(price ~ origin + store + origin:store, data=mydata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=mydata)
summary(fit4)


# fit4 is the same as fit3
# just anothe format


# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений
# на урожайность гороха (yield). Нашей задачей будет выяснить, существенно ли одновременное 
# применение азота (фактор N) и фосфата (фактор P). Примените дисперсионный анализ, 
# где будет проверяться влияние фактора применения азота (N), 
# влияние фактора применения фосфата (P) и их взаимодействие.
# В ответе укажите p-value для взаимодействия факторов N и P.
View(npk)

npk_yield <- aov(yield ~ N * P, data=npk)
summary(npk_yield)


# Теперь проведите трехфакторный дисперсионный анализ, где зависимая переменная - это урожайность (yield),
# а три фактора - типы удобрений (N, P, K). 
# После проведения данного анализа вы получите три значения p - уровня значимости 
# (о значимости каждого из факторов).
# Соотнесите названия факторов и значения p - уровня значимости.

npk_yield_3x <- aov(yield ~ N + P +K, data=npk)
summary(npk_yield_3x)



# Pairwise comparisons

ggplot(mydata, aes(x = food, y = price)) + 
  geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)

?TukeyHSD
# Compute Tukey Honest Significant Differences
# Create a set of confidence intervals on the differences between the means of the levels
# of a factor with the specified family-wise probability of coverage. 
# The intervals are based on the Studentized range statistic, Tukey's ‘Honest Significant Difference’ method

TukeyHSD(fit5)


# Проведите однофакторный дисперсионный анализ на встроенных данных iris. 
# Зависимая переменная - ширина чашелистика (Sepal.Width), 
# независимая переменная - вид (Species). Затем проведите попарные сравнения видов. 
# Какие виды статистически значимо различаются по ширине чашелистика (p < 0.05)?

View(iris)
fit6 <- aov(Sepal.Width ~ Species, data=iris)
summary(fit6)

TukeyHSD(fit6)

# virginica и versicolor
# virginica и setosa
# versicolor и setosa


# Repeated measures

mydata2 <- read.csv('therapy_data.csv')
str(mydata2)
View(mydata2)
mydata2$subject <- as.factor(mydata2$subject)


fit1 <- aov(well_being ~ therapy, data = mydata2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit1b)


fit2 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit2)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being)) + 
  geom_boxplot() + 
  facet_grid(~subject)


fit3 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit3)
fit3b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit3b)


# В этой задаче вам дан набор данных, в котором представлена информация о температуре нескольких пациентов, 
# которые лечатся разными таблетками и у разных врачей.
# Проведите однофакторный дисперсионный анализ с повторными измерениями: влияние типа таблетки (pill) 
# на температуру (temperature) с учётом испытуемого (patient). Каково p-value для влияния типа таблеток на температуру?
# 
# Данные: https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv
# 
# Не забудьте, важно перевести переменную patient в фактор!  

df <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv'))
df$patient <- as.factor(df$patient)
summary(aov(temperature ~ pill + Error(patient/pill), data = df))

# Теперь вашей задачей будет провести двухфакторный дисперсионный анализ с повторными измерениями: 
# влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature. 
# Учтите обе внутригрупповые переменные: и тот факт, что один и тот же больной принимает разные таблетки, 
# и тот факт, что  один и тот же больной лечится у разных врачей! Каково F-значение для взаимодействия 
# факторов доктора (doctor) и типа таблеток (pill)?
# Данные: https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv

df <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv'))
df$patient <- as.factor(df$patient)
summary(aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = df))

# Вспомните графики из лекций и дополните шаблон графика в поле для ответа так (не добавляя еще один geom) ,
# чтобы объединить линиями точки, принадлежащие разным уровням фактора supp.
# Не забудьте подключить нужный для построение графика пакет.
# Пожалуйста, сохраните график в переменную obj.

library(ggplot2)
View(ToothGrowth)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
