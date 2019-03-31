# control statements

mydata <- read.csv('D:/OneDrive/education/R/evals.csv')


# if

a <- 0

if (a > 0){
  print('positive')
} else {
  print('not positive')
}


if (a > 0){
  print('positive')
} else print('not positive')


if (a > 0){
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')



# ifelse

a <- 10
ifelse(a > 0, 'positive', 'not positive')

a <- c(1, -1)
ifelse(a > 0, 'positive', 'not positive')


# for 

for (i in 1:100){
  print(i)
}


for (i in 1:nrow(mydata)){
  print(mydata$score[i])
}



# for + if
for (i in 1:nrow(mydata)){
  if (mydata$gender[i] == 'male'){
    print(mydata$score[i]) 
  }
}



# for + if  VS  ifelse

mydata$quality <- rep(NA, nrow(mydata))

for (i in 1:nrow(mydata)){
  if (mydata$score[i] > 4){
    mydata$quality[i] <- 'good'
  } else mydata$quality[i] <- 'bad'
}


mydata$quality2 <- ifelse(mydata$score > 4, 'good', 'bad')


# while

i <- 1

while(i < 51){
  print(mydata$score[i])
  i <- i+1
}


###

# Создайте новую числовую переменную  new_var в данных mtcars,
# которая содержит единицы в строчках,
# если в машине не меньше четырёх карбюраторов (переменная "carb")
# или больше шести цилиндров (переменная "cyl").
# В строчках, в которых условие не выполняется, должны стоять нули.

data(mtcars)

mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)

mtcars$new_var <- rep(NA, nrow(mtcars))
for (i in 1:nrow(mtcars)) {
  if (mtcars$carb[i] >= 4) {
    mtcars$new_var[i] <- 1
  } else if (mtcars$cyl[i] >6){
    mtcars$new_var[i] <- 1
  } else mtcars$new_var[i] <- 0}


mtcars$new_var <- 0 + (mtcars$carb >= 4 | mtcars$cyl > 6)

#author = Michael Gordon
add_new_col <- function(a, b)
{
  # function to add a new column to mtcars
  # https://stepik.org/lesson/Элементы-синтаксиса-11478/step/4?course=Анализ-данных-в-R&unit=2520
  if ((a >= 4 ) | ( b > 6)) 
  { return(1) }
  else
  { return(0) }
}

# but it`s not vectorized yet
# if state wont work properly
# let`s use Vectorize wrapper

add_new_col_vect <- Vectorize(add_new_col)
#now its vectorized

mtcars$new_var <- add_new_col_vect(mtcars$carb, mtcars$cyl)


# В уже существующей переменной my_vector сохранен вектор из 50 чисел.
# Решите задачу используя конструкцию:
# if () {
#  } else {
#  } 
# Если среднее значение вектора my_vector больше 20,
# в переменную result сохраните "My mean is great",
# если среднее значение my_vector меньше или равно 20
# то в переменную result сохраните  строку "My mean is not so great".

my_vector <- c(20.67, 23.34, 22.65, 17.11, 22.1, 26.32, 20.39, 21.04, 23.78, 31.11, 21.13, 22.44, 23.21, 27.02, 18.64, 20.9, 20.77, 20.0, 21.29, 23.48, 18.47, 25.02, 17.04, 30.97, 12.91, 23.88, 32.95, 8.46, 23.15, 21.05, 20.63, 19.95, 17.38, 29.35, 24.43, 23.66, 18.32, 30.13, 19.36, 19.67, 24.23, 20.82, 18.21, 9.91, 21.45, 18.04, 18.31, 17.18, 10.99, 10.06)
mean(my_vector)

if (mean(my_vector) > 20.0) {
  result <- "My mean is great" } else {
  result <- "My mean is not so great" }
print(result)

result <- if (mean(my_vector)>20) {
  print("My mean is great")
} else { print ("My mean is not so great")
} 

result <- if (mean(my_vector) > 20) "My mean is great" else "My mean is not so great"
print(result)



# В этой задаче от вас потребуется узнать некоторую информацию о типах данных в R самостоятельно!
# Встроенные в R данные AirPassengers - это новый для нас формат данных типа Time-Series.
# Изучите структуру этих данных, прежде чем начать решение задачи! Например напишите команды:
# ?AirPassengers # справка о данных
# str(AirPassengers) # структура данных
# В встроенных в R данных AirPassengers хранится 144 значения (количество пассажиров в месяц)
# с 1949 по 1960 год. Данные Time-Series очень похожи на вектор по своей структуре,
# например мы можем обратиться к любому из 144 элементов используя уже знакомую нам индексацию
# AirPassengers[1] или AirPassengers[56].
# Можно вообще перевести исходные данные в вектор при помощи команды as.vector(AirPassengers)
# и продолжить с ними работу как с вектором.
# И так ваша задача создать переменную good_months и сохранить в нее число пассажиров только в тех месяцах,
# в которых это число больше, чем показатель в предыдущем месяце.  
# Важный момент! В R оператор : для создания последовательности имеет приоритет
# над арифметическими действиями. Таким образом, если у вас есть переменная i, равная 10,
# и вы хотите создать вектор от 1 до i - 1, воспользуйтесь скобками, чтобы указать последовательность действий.
#
# i <- 10
# 1 : i - 1 # так мы создадим последовательность от 1 до 10, а потом вычтем единицу из каждого элемента
# [1] 0 1 2 3 4 5 6 7 8 9
#
# 1 : (i - 1) # а вот так мы создадим последовательность от 1 до i - 1, то есть от 1 до 9. 
# [1] 1 2 3 4 5 6 7 8 9

data(AirPassengers)
?AirPassengers
str(AirPassengers)
as.vector(AirPassengers)
print(AirPassengers)

AirPassengers <- as.vector(AirPassengers)

good_months <- c()
for (i in 2:length(AirPassengers)) {
  if (AirPassengers[i] > AirPassengers[i-1]){
    good_months<-c(good_months, AirPassengers[i])
  }
}
print(good_months)
#
good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 
#
good_months <- AirPassengers[c(FALSE, AirPassengers[2:144] > AirPassengers[1:143])]
#
good_months <- AirPassengers[which(AirPassengers[-1] - AirPassengers[-length(AirPassengers)] > 0)+1]
print(good_months)
#
vector1 <- as.vector(AirPassengers)
vector2 <- c(vector1[length(vector1)],vector1[-length(vector1)])
diffs <- vector1-vector2
output = vector1[diffs>0]
good_months <- output
print(good_months)
#Пример решения с циклом:
  
good_months <- c()    
index <- 1    
for (i in 2:length(AirPassengers)) {    
  if (AirPassengers[i]>AirPassengers[i-1]){    
    good_months[index] <- AirPassengers[i]    
    index <- index + 1    
  }    
}
#
vAP <- as.vector(AirPassengers)
vAP1 = vAP[1:length(vAP)-1]
vAP2 = vAP[2:length(vAP)]
good_months = vAP2[vAP2 > vAP1]
print(good_months)
#
ifgood <- c(FALSE, AirPassengers[2:144] > AirPassengers[1:143])
good_months <- AirPassengers[ ifgood ]
print(good_months)
#
AP_minus1 <- AirPassengers[1 : 143]
AP        <- AirPassengers[2 : 144]
good_months <- AP[AP - AP_minus1 > 0]
print(good_months)

# Для встроенных в R данных AirPassengers рассчитайте скользящее среднее с интервалом сглаживания равным 10.
# Напечатайте получившийся результат (первым значением в выводе должно быть среднее для элементов 1:10,
# во втором значении - среднее для элементов 2:11 и т.д., в последнем  - среднее для элементов 135 :144)
# Все полученные значения средних сохраните в переменную moving_average.
print(length(AirPassengers)-10)

moving_average <- c()
MA <- c()
for (i in 1:(length(AirPassengers)-9)) {
    MA <- c(AirPassengers[i],AirPassengers[i+1],AirPassengers[i+2],AirPassengers[i+3],AirPassengers[i+4],AirPassengers[i+5],AirPassengers[i+6],AirPassengers[i+7],AirPassengers[i+8],AirPassengers[i+9])
    moving_average<-c(moving_average, mean(MA))
    MA <- c()
}
print(moving_average)

#
moving_average <- numeric(135) # создаем пустой числовой вектор из 135 элементов    
last_index <- length(AirPassengers) - 9    
for (i in 1:last_index) {    
  end <- i + 9    
  moving_average[i] <- mean(AirPassengers[i:end])    
}    

#Можно решить и без цикла при помощи разностей кумулятивных сумм!    
  
n <- 10    
d <- AirPassengers    
cx <- c(0, cumsum(d))    
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n

#

AirP <- as.vector(AirPassengers)
moving_average  <- c()    
for (i in 1:135) {moving_average [i] <- mean(AirP[i:(i+9)])}
print(moving_average)
