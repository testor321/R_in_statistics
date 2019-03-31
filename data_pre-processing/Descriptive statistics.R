#Step 2: Data preprocessing

?mtcars

df  <- mtcars

str(df)
View(df)

# изменение типа представления
# num -> factor 
df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


#Step 3: Descriptive statistics
# медиана / Miles/(US) gallon
median(df$mpg)
# среднее / Displacement (cu.in.) / объем двигателя
mean(df$disp)
# средне-квадратичное отклонение / Gross horsepower / лошадиный сил
sd(df$hp)
# размах / Number of cylinders
range(df$cyl)

mean_disp  <- mean(df$disp)

# среднее / расход топлива для двигателей с 6 цилиндрами
mean(df$mpg[df$cyl == 6])
# среднее / расход топлива для двигателей с 6 цилиндрами И тип двигателя == V
mean(df$mpg[df$cyl == 6 & df$vs == "V"])
# средне-квадратичное отклонение / для мощьности (лошадиный сил) - цилиндров НЕ 3  И коробка передач Auto
sd(df$hp[df$cyl != 3 & df$am == "Auto"])



# mtcars
# Рассчитайте среднее значение времени разгона (qsec) для автомобилей,
# число цилиндров (cyl) у которых не равняется 3
# и показатель количества миль на галлон топлива (mpg) больше 20.
# Получившийся результат (среднее значение) сохраните в переменную result.

result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg>20])


#Step 5: Aggregation

# Compute Summary Statistics of Data Subsets
?aggregate

df  <- mtcars
str(mtcars)
?mtcars

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

mtcars$new_var <- NULL
df$new_var <- NULL


# вычислить среднюю мощьность / группировка по типу двигателя
mean_hp_vs  <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)
mean_hp_vs

# задать названия столбцов
colnames(mean_hp_vs)  <- c("VS", "Mean HP")

# альтернативная форма записи
aggregate(hp ~ vs, df, mean)

# группировка по двум параметрам vs and am
aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

# медиана для всех столбцов кроме 8 и 9
# группировка по типу коробки передач
aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)

# среднеквадратичное отклонение для 1 и 3 столбца 
# группировка по am / vs
aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
my_stats

####
aggregate(. ~ am, mtcars[, -c(8, 10)], median)
#таким образом, мы рассчитаем медиану в двух группах по переменной am для все переменных кроме 8 (vs) и 10 (cyl).

# aggregate(. ~ am, mtcars, mean) # рассчитать среднее всех переменных, сгруппированных по am
# aggregate(am ~ ., mtcars, mean) # рассчитать среднее значение переменной am, сгруппированной по всем остальным переменным.


# При помощи функции aggregate рассчитайте стандартное отклонение переменной hp (лошадиные силы)
# и переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач. 
# Полученные результаты (результаты выполнения функции aggregate) сохраните в переменную descriptions_stat.

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)
#
descriptions_stat <- aggregate(x = mtcars[,c(3,4)],by = list(mtcars$am),FUN = sd)
#
descriptions_stat <- aggregate(. ~ mtcars$am,mtcars[,c(3,4)],sd)
descriptions_stat
#Step 8, 9: Library "psych"

# Tools -> Install Packages
# psych,ggplot2

# install.packages('ggplot2')
# install.packages('psych')

library(psych)

?describe

describe(x = df)

# Basic descriptive statistics / group = df$vs
descr  <- describe(x = df[,-c(8,9)])

?describeBy

# Basic summary statistics by group / group = df$vs / exclude -c(8,9) : vs, am
descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs)


descr2
# show only V
descr2$V
#show only S
descr2$S

# матрица, 1 знак после запятой
descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)
# fast = TRUE, рассчитываются только базовае статистики
descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)
# разбивка по 2 и более группирующим переменным : list(df$vs, df$am)
describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, fast = T,  mat = T)


#Step 10: NA values

# check is NA exist in the data
# calculate amount of NA 
is.na(df$mpg)
sum(is.na(df))

# set NA for the first 10 values for df$mpg
df$mpg[1:10]  <- NA

# if NA exist, the remove them from calculation
mean(df$mpg, na.rm = T)

# aggregate ignores NA by default
aggregate(mpg ~am, df, sd)

# by default removes NA elements
# if na-rm = T, then all ROWs with NA elements will be removed from calculation
describe(na.rm = )



#

# Воспользуемся встроенными данными airquality.
# В новую переменную сохраните subset исходных данных, оставив наблюдения только для месяцев 7, 8 и 9.
# При помощи функции aggregate рассчитайте количество непропущенных наблюдений по переменной
# Ozone в 7, 8 и 9 месяце. Для определения количества наблюдений используйте функцию length().
# Результат выполнения функции aggregate сохраните в переменную result.

library(dplyr)
?airquality
View(airquality)

df <- subset(airquality, Month%in%c(7,8,9))
result <- aggregate(Ozone ~ Month,df,length)

result <- aggregate(Ozone ~ Month, airquality, subset = Month %in% c(7,8,9), length) 

result


# Примените функцию describeBy к количественным переменным данных airquality,
# группируя наблюдения по переменной Month.
# Чему равен коэффициент асимметрии (skew) переменной Wind в восьмом месяце?

describeBy(x = airquality, group = airquality$Month)

z <- subset(airquality,Month==8)
skew(z$Wind)

# Обратимся к встроенным данным iris. Соотнесите значения стандартного отклонения переменных.
describe(x = iris)
describe(iris)['sd']

# Как посчитать sd для всех переменных за раз? 
lapply(subset(iris, select = 1:4), FUN = sd)

sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)



# В данных iris расположите по убыванию значения медиан количественных переменных в группе virginica.
str(iris)
describe(x = iris)
View(iris)
describeBy(x = iris, group = iris$Species)


describe(iris[iris$Species == 'virginica', 1:4])['median']
describeBy(iris[,1:4], group = iris$Species)$virginica['median']


# В переменной my_vector сохранен вектор с пропущенными значениями.
# Вам нужно создать новый вектор fixed_vector, в котором все пропущенные значения вектора 
# my_vector будут заменены на среднее значение по имеющимся наблюдениям.
# При этом исходный вектор оставьте без изменений!
# Напоминаю, переменная my_vector уже создана, сразу начинайте работать с ней. 
# Перед тем, как сдавать решение, вы можете потренироваться на различных примерах. 
# Ниже небольшой код, который может создать случайный вектор (выборка из нормального распределения) 
# с пропущенными значениями.

# my_vector <- rnorm(30)
# my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA

# Задача для самостоятельной работы:
# Изучите справку по функции replace. Вызвать справку можно исполнив команду:
# ?replace
# Попробуйте решить это задание при помощи этой функции.
#
# Sample Input 1:
# 23 10 16 19 23 22 16 21 24 20 22 21 19 25 22 14 22 14 16 15 NA 24 NA NA NA 23 15 21 24 NA NA NA 18 21 18 NA 17 20 17 NA
# Sample Output 1:
# 23.0 10.0 16.0 19.0 23.0 22.0 16.0 21.0 24.0 20.0 22.0 21.0 19.0 25.0 22.0 14.0 22.0 14.0 16.0 15.0 19.4194 24.0 19.4194 19.4194 19.4194 23.0 15.0 21.0 24.0 19.4194 19.4194 19.4194 18.0 21.0 18.0 19.4194 17.0 20.0 17.0 19.4194
# Sample Input 2:
# 27 19 17 20 20 20 15 28 17 9 17 23 17 14 24 15 25 21 21 21 16 NA 21 23 NA NA 20 NA 25 NA 19 19 16 NA NA NA NA NA NA NA
# Sample Output 2:
# 27.0 19.0 17.0 20.0 20.0 20.0 15.0 28.0 17.0 9.0 17.0 23.0 17.0 14.0 24.0 15.0 25.0 21.0 21.0 21.0 16.0 19.6071 21.0 23.0 19.6071 19.6071 20.0 19.6071 25.0 19.6071 19.0 19.0 16.0 19.6071 19.6071 19.6071 19.6071 19.6071 19.6071 19.6071

my_vector <- c(23 , 10 , 16 , 19 , 23 , 22 , 16 , 21 , 24 , 20 , 22 , 21 , 19 , 25 , 22 , 14 , 22 , 14 , 16 , 15 , NA , 24 , NA , NA , NA , 23 , 15 , 21 , 24 , NA , NA , NA , 18 , 21 , 18 , NA , 17 , 20 , 17 , NA)
fixed_vector <- my_vector
fixed_vector[is.na(fixed_vector)] <- mean(fixed_vector, na.rm=TRUE)

fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))


#
fixed_vector <- ifelse(is.na(my_vector), mean(my_vector, na.rm = T), my_vector)
#
fixed_vector <- replace(my_vector, my_vector %in% NA, mean(my_vector,na.rm = T))
