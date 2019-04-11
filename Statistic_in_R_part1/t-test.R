library(ggplot2)
?iris
df  <- iris
View(df)
str(df)

df1  <- subset(df, Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x =Sepal.Length ))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species ))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

?shapiro.test
# Shapiro-Wilk Normality Test
# Performs the Shapiro-Wilk test of normality.
shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

#The same result in one line
by(df1$Sepal.Length, INDICES = df1$Species, shapiro.test)

?bartlett.test
# Bartlett Test of Homogeneity of Variances
# Performs Bartlett's test of the null that the variances in each of the groups (samples) are the same.

bartlett.test(Sepal.Length  ~ Species, df1)

# критерий Левина - r library car
library(car)


t.test(Sepal.Length  ~ Species, df1)
test1  <- t.test(Sepal.Length  ~ Species, df1)

str(test1)
test1$p.value

#error
t.test(Sepal.Length  ~ Species, df, var.equal = T)
#ok
t.test(Sepal.Length  ~ Species, df1, var.equal = T)
# 
t.test(df1$Sepal.Length, mu = 8)


t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

# Памятка
# t-Критерий Стьюдента для независимых выборок
# t.test(Var1 ~ Var2, data) # если первая переменная количественная, а вторая фактор
# t.test(data$Var1, data$Var2) # если обе переменные количественные
# t-Критерий Стьюдента для зависимых выборок
# t.test(data$Var1, data$Var2, paired = T)
# Проверка на нормальность распределения
# shapiro.test(Var1) # проверка на нормальность распределения переменной Var1
# но не удобно когда есть группирующая факторная переменная
# Поможет функция by(), которая применяет различные функции на каждом уровне фактора.  
# by(iris$Sepal.Length, INDICES = iris$Species, shapiro.test) # проверка на нормальность переменной 
# Sepal.Length в трех разных группах в соответствии с переменной Species
# Проверка на гомогенность дисперсий
# bartlett.test(mpg ~ am, mtcars) #Критерий Бартлетта 


# Воспользуемся еще одним встроенным набором данных в R  - ToothGrowth.
# Данные позволяют исследовать рост зубов у морских свинок в зависимости от дозировки
# витамина C и типа потребляемых продуктов.
# Сравните среднее значение длины зубов свинок, которые потребляли
# апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, со средним значением длины зубов свинок,
# которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 
# Значение t - критерия сохраните в переменную t_stat.
# https://stepik.org/lesson/11504/step/10?discussion=850149&thread=solutions&unit=2527
df<-ToothGrowth 
df1 <- subset(df, dose == 0.5 & supp == "OJ") 
df2<- subset(df, dose == 2 & supp == "VC") 
df3 <- rbind(df1, df2) 
t_stat<-t.test(len ~ supp, df3, var.equal = T)$statistic 
#
correct_data <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)    
t_stat <- t.test(len ~ supp, correct_data)$statistic
#
#Сравните среднее значение длины зубов свинок, которые потребляли апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, со средним значением длины зубов свинок, которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 
# with explanation / step-by-step
?ToothGrowth    
str(ToothGrowth)

hist(ToothGrowth$len)
#распределение с натяжкой нормальное
library(ggplot2)

ggplot(ToothGrowth, aes(x = len ))+
  geom_histogram(fill = "white", col = "black", binwidth = 4)+
  facet_grid(supp ~ dose)

#распределение но дозами по соку и по витамину С. Видно, что сок более эффективен

ggplot(ToothGrowth, aes(len, fill = supp))+
  geom_density(alpha = 0.5)

#видно что эффект от сока больше

ggplot(ToothGrowth, aes(supp, len))+
  geom_boxplot()
#выбросов нет

shapiro.test(ToothGrowth$len)
by(ToothGrowth$len, INDICES = ToothGrowth$supp, shapiro.test)

#общий тест на нормальность пойден, а вот отдельно для группы потребляющей сок распределение нормальным назвать нельзя

hist(ToothGrowth$len[ToothGrowth$supp=='OJ'])
hist(ToothGrowth$len[ToothGrowth$supp=='VC'])

#проверяю визуально, и подтверждаю, что распределение для сока назвать нормальным сложно

bartlett.test(len  ~ supp, ToothGrowth)
#дисперисия гомогенна

#нализируем сабсет

by(df$len, INDICES = df$supp, shapiro.test)
#группы проходят тест на нормальность распределения

ggplot(df, aes(len, fill = supp))+
  geom_density(alpha = 0.5)
ggplot(df, aes(supp, len))+
  geom_boxplot()
#визуальный анализ подтверждает значимое различие

df <- rbind(subset(ToothGrowth, supp == 'OJ' & dose == 0.5), subset(ToothGrowth, supp == 'VC' & dose == 2))
t.stat <- t.test(len ~ supp, df)$statistic
#средние по группам значино различаются 
# with explanation / step-by-step



##
# Скачайте данные, посвященные влиянию различного типа лечения на показатель артериального давления. 
# https://stepic.org/media/attachments/lesson/11504/lekarstva.csv
# По всем испытуемым сравните показатель давления до начала лечения (Pressure_before) 
# с показателем давления после лечения (Pressure_after) при помощи t - критерия для зависимых выборок. 
# В поле для ответа укажите значение t - критерия.
# (В качестве десятичного разделителя используйте запятую, например: 123,54)

# Categorical data
setwd("D:/OneDrive/education/R/Statistic_in_R_part1")
# grants.csv : статистика одобрения заявок на научные исследования в Австралийском университете
df <- read.csv("lekarstva.csv")
# df <- read.csv(url('https://stepic.org/media/attachments/lesson/11504/lekarstva.csv'))
t.test(df$Pressure_before, df$Pressure_after, paired = T)$statistic 

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", 
               size = 2)


?wilcox.test

test2  <- wilcox.test(Petal.Length ~ Species, df1)
pv  <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()


wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest  <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest$p.value


# В этом задании нужно проверить гипотезу о равенстве средних двух выборок, 
# загрузив набор данных (нажмите начать решать задание) и выполнив все необходимые операции 
# на вашем компьютере. В скачанных данных вы найдете две переменные: количественную переменную,
# и номинативную переменную с двумя градациями (которая разделяет наблюдения на две группы).
# Для того чтобы без труда прочитать скачанные данные воспользуйтесь функцией:
# read.table("dataset_11504_11.txt")
# разумеется, у вас может быть другое название файла
# также убедитесь, что файл находится в рабочей директории
# или укажите полный путь к файлу
# Сначала с помощью теста Бартлетта проверьте гомогенность дисперсий двух выборок.
# В случае, если дисперсии значимо не отличаются (с уровнем 0.05), примените тест Стьюдента, 
# иначе - непараметрический тест (Манна-Уитни). В поле для ответа введите получившийся p-value,
# с точностью четыре знака после запятой.
# Обратите внимание, что по умолчанию в t.test стоит var.equal = FALSE, 
# так как мы будем применять его только в случае гомогенности дисперсий, измените значение 
# этого параметра на  var.equal = TRUE.
# Каждый раз вы будете скачивать новый набор данных.
# Важно - в этом ответе используйте точку как десятичный разделитель!
# Если p - value сильно меньше 0.05, например, 1.01e-07, в поле для ответа можете ввести 0
# Сам код в поле для ответа вводить не надо, от вас ожидается только результат - одно число
# Примеры ответа:
# 0.0424
# 0
# 0.9
setwd("D:/OneDrive/education/R/Statistic_in_R_part1")
# TBD
df <- read.table("dataset_11504_15.txt") 
# сохраняем в df сразу через read.table (не забывает про кавычки и указание расширения
# далее bartlet.test
# далее если p.value > 0.05 делаем t.test ,
# если < 0.05 делаем wilcox.test
p <- bartlett.test(V1  ~ V2, df)$p.value
p

ifelse((p>=0.05),t.test(V1 ~ V2,df,var.equal = TRUE)$p.value,wilcox.test(V1 ~ V2,df)$p.value)

if (p > 0.05) { 
  p_res <- t.test(V1~V2,p,var.equal = TRUE)
} else { 
  p_res <- wilcox.test(V1 ~ V2,df)
}
formatC(p_res$p.value,4)

unlink ( "dataset_11504_15.txt" )

#
check_ds <- function(x){
  if(bartlett.test(x$V1, x$V2)$p.value < 0.05) {
    print("We do Wilcox test")
    wilcox.test(V1 ~ V2, x)$p.value
  } else {
    print("We do T test")
    t.test(V1 ~ V2, x, var.equal = T)$p.value
  }
}
formatC(check_ds(df),4)


#

# В данных сохранены две количественные переменные, проверьте гипотезу о равенстве
# средних этих переменных при помощи t- теста для независимых выборок.
# Если обнаружены значимые различия (p< 0.05), то введите через пробел три числа:
# среднее значение первой переменной, среднее значение второй переменной, p - уровень значимости. Например:
# 22.45 12.56 0.04
# Если значимые различия не обнаружены, то в поле для ответа введите: 
# "The difference is not significant"
# В этой задаче оставьте var.equal = FALSE


# TBD
df <- read.table("dataset_11504_16.txt") 
p_res <- t.test(df$V1, df$V2, var.equal = FALSE)$p.value 
p_res
if (p_res < 0.05) { 
  p_res <- t.test(df$V1, df$V2) 
  cat(paste(c(sapply(df,mean), p_res$p.value), collapse = "\n")) 
} else { 
  "The difference is not significant" 
}
