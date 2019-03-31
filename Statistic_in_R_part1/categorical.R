# Categorical data
setwd("D:/OneDrive/education/R/Статистика в R. Часть 1")

# grants.csv : статистика одобрения заявок на научные исследования в Австралийском университете
df <- read.csv("grants.csv")

str(df)



df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))
?factor


# 1d Table 
t1 <- table(df$status)
t1
# Not funded     Funded 
# 747        673 

# ппосмотреть размерность таблицы
dim(t1)


# 2d Table
#  таблица сопряженности по 2м переменным
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)

dim(t2)

# пропорции / проценты
prop.table(t2)
# 100%  - сумма по строке
prop.table(t2, 1)
# 100%  - сумма по столбцу
prop.table(t2, 2)

?prop.table


# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)


# К частям таблицы можно обращаться так же, как и к матрицам. 
# HairEyeColor - таблица с данными, встроенными в R. Посмотрите на неё в R.
# Команда dimnames(HairEyeColor) позволит нам посмотреть, какие измерения есть в этой таблице 
# и как они называются. Например, чтобы обратиться к части таблицы, 
# в которой хранятся данные только о мужчинах, нам нужно выполнить следующую команду: 
# HairEyeColor[ , ,'Male']
# Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) от общего числа голубоглазых мужчин.
# Обратите внимание, что нужны не проценты, а просто доля, то есть десятичная дробь  (например, не 10%, а 0.1).

dimnames(HairEyeColor)
HairEyeColor[ 'Red', 'Blue','Male']

red_men <- prop.table(HairEyeColor[, , 'Male'], 2)['Red', 'Blue']
red_men <- prop.table(HairEyeColor[,"Blue" ,"Male" ] )["Red"]
red_men <- HairEyeColor["Red", "Blue", "Male"] / sum(HairEyeColor[, "Blue", "Male"])
red_men


a <- prop.table(HairEyeColor[,,"Male"],2)
#создаем двумерную таблицу, 2- группировка значений по столбцу
red_men <- a['Red', 'Blue']
#просто обращаемся к ее х/у

# С таблицами, как и с матрицами, можно совершать разные арифметические операции,
# например, суммировать все элементы таблицы.
# Напишите число зеленоглазых женщин в наборе данных HairEyeColor.
dimnames(HairEyeColor)
sum(HairEyeColor[, "Green", "Female"])
#31

# plots

barplot(t1)
barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

##########################

# Binomial Test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)


# Chi-Square
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs


t2
chisq.test(t2)



# Fisher's Exact Test

fisher.test(t2)
