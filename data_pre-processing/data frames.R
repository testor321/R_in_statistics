# Памятка 
# арифметические и логические операции:
# Арифметические операции
#
#
# + сложение
# - вычитание
# * умножение
# / деление  (5 / 2 = 2.5)
# ^ или **  возведение в степень (5^2 = 25 или 5**2 = 25)
# x %% y остаток от деления  (5 %% 2 = 1)
# x %/% y целая часть от деления (5 %/% 2 = 2)
#
# Логические операции
#
#
# < меньше 
# <= меньше или равно
# > больше
# >= больше или равно
# == проверка на равенство
# != не равно
# !x не x
# x | y x или y
# x & y x и y
#
# TRUE  можно сокращенно обозначать T
# FALSE можно сокращенно обозначать F


# Reading data

?read.table
?read.csv


mydata <- read.csv('D:/OneDrive/education/R/evals.csv')


# Summaries

head(mydata, 3)
tail(mydata)

View(mydata)

# Structure
str(mydata)

a <- names(mydata)

summary(mydata)




# Variables

b <- mydata$score

mean(mydata$score)

summary(mydata$score)

mydata$score * 2

# add new column into dataframe
mydata$ten_point_scale <- mydata$score * 2



summary(mydata$ten_point_scale)

# создать новый столбец с нулями
mydata$new_varible <- 0
# создать новый столбец и заполнить числами по возрастанию
mydata$number <- 1:nrow(mydata)
summary(mydata$number)

# количество строк в датасете
nrow(mydata)
# количество столбцов в датасете
ncol(mydata)





# Subsetting

mydata$score[1:10]

mydata[1,1]
mydata[c(2,193,225),1]
mydata[101:200,1]

mydata[5,]
mydata[,1] == mydata$score

mydata[,2:5]
head(mydata[,2:5])

##


# Subsetting with condition

mydata$gender
mydata$gender == 'female'
head(mydata[mydata$gender == 'female',1:3])
# оценки только для женщин
mydata[mydata$gender == 'female',1]

head(subset(mydata, gender == 'female'))
head(subset(mydata, score > 3.5))



# rbind, cbind
# row bind / column bind

mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:24]
mydata7 <- cbind(mydata6, mydata5)



###

library(help = "datasets")
# Команда data(mtcars) добавит датасет в рабочую среду. 
# Команда help(mtcars) выведет информацию о датасете.
# Команда my_data <- mtcars запишет датасет в новую переменную. 

data(mtcars)
help(mtcars)
my_data <- mtcars

# Чтобы изучить структуру данных воспользуйтесь командой str()
str(mtcars)

# Чтобы отобрать нужные колонки (переменные) в данных вы можете:
# - использовать номера колонок (не забудьте обернуть индексы в вектор):
mtcars[, c(1, 3, 4)] 

# использовать имена колонок:
mtcars[, c("mpg", "hp")]

# Чтобы отобрать нужные строки в данных:
mtcars[c(1, 5, 7), ]

# Эти приемы можно комбинировать:
mtcars[c(1, 4, 5), c(1, 4)] 

# Запомните, сначала идут индексы строк, потом индексы колонок!
# Также обратите внимание, что мы можем использовать отрицательную индексацию,
# чтобы отобрать все колонки/строки кроме указанных:
mtcars[, -c(3, 4)] # отберем все строчки и все колонки кроме 3 и 4. 

# Для более сложных запросов используйте функцию subset():
subset(mtcars, hp > 100 & am == 1)

# Добавить новую переменную можно при помощи оператора $
mtcars$new_var <- 1:32

# Чтобы удалить переменную из данных, используйте такую конструкцию:
mtcars$new_var <- NULL


# В датафрэйме mtcars создайте новую колонку (переменную) под названием even_gear
# в которой будут единицы, если значение переменной (gear) четное, и нули если количество нечетное.   
data(mtcars)
mtcars$even_gear <- (mtcars$gear + 1) %% 2
#
mtcars$even_gear <- as.numeric(mtcars$gear%%2 == 0)
#
mtcars$even_gear <-  1 - mtcars$gear %% 2
#
mtcars$even_gear <- ifelse (mtcars$gear %% 2 == 1, 0, 1 )
#
mtcars$even_gear[mtcars$gear %% 2 == 0] <- 1
mtcars$even_gear[mtcars$gear %% 2 != 0] <- 0
#
mtcars$even_gear <- abs((mtcars$gear %% 2) - 1)


# Теперь ваша задача создать переменную - вектор mpg_4
# и сохранить в нее значения расхода топлива (mpg) для машин с четырьмя цилиндрами (cyl). 

mpg_4 <- mtcars[mtcars$cyl == 4, 1]
#
mpg_4 <-mtcars$mpg[mtcars$cyl==4]
#
mpg_4 <- mtcars[mtcars$cyl == 4, "mpg"]
#
mpg_4 <- subset(mtcars, cyl == 4)$mpg
#
mpg_4 <- mtcars[ mtcars$cyl==4, mtcars$mpg ]

# Ваша задача создать новый dataframe под названием mini_mtcars, в котором будут сохранены только 
# третья, седьмая, десятая, двенадцатая и последняя строчка датафрейма mtcars.


mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)),]

