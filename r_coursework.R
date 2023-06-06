library(ggplot2)
library(scales)
library(tseries)
library(ggpubr)
theme_set(theme_pubr())
url = "https://covid19.who.int/WHO-COVID-19-global-data.csv"

df = read.csv(url)
df = subset(df, Country_code == "RU")
df = df[c("Date_reported", "New_cases")]
df$Date_reported = as.Date(df$Date_reported, format = "%Y-%m-%d")
df = df[92:(length(df$Date_reported)-16),]
ggplot(df, aes(Date_reported, New_cases)) + xlab("Дата") + ylab("Количество зараженных за день") +
  geom_line()

adf.test(df$New_cases)
kpss.test(df$New_cases)
acf(df$New_cases)
pacf(df$New_cases)
summary(pacf(df$New_cases))

ar_predicte_model = function(window_vector) {
  pair_list = list(xi = c(), xi_1 = c())
  for (j in 2:length(window_vector)) {
    pair_list$xi = c(pair_list$xi, window_vector[j])
    pair_list$xi_1 = c(pair_list$xi_1, window_vector[j-1])
  }
  yt_lm = lm(data = pair_list, formula = xi ~ xi_1)
  return(yt_lm$coefficients[2] * window_vector[length(window_vector)] + yt_lm$coefficients[1])
}

yt_train = function(df, k) {
  
  window_vector = c()
  for (i in k:length(df$New_cases)) {
    window_vector= c(window_vector, df$New_cases[((i - (k-1)):i)]) 
  }
  window_matrix = matrix(window_vector, ncol = k, byrow = T)
  
  yt_vec = c()
  for (i in 1:nrow(window_matrix)) {
    yt = ar_predicte_model(window_matrix[i,])
    yt_vec = c(yt_vec, yt)
  }
  return(yt_vec)
}

window_vector = c()
for (i in k:length(df$New_cases)) {
  window_vector= c(window_vector, df$New_cases[((i - (k-1)):i)]) 
}
window_matrix = matrix(window_vector, ncol = k, byrow = T)

k = 100

ar_train =  data.frame(New_cases = yt_train(df, k), Date_reported = df$Date_reported[k:length(df$Date_reported)]) 
ggplot(df, aes(x = Date_reported, y = New_cases, colour = "Исходные данные")) + xlab("Дата") + ylab("Количество зараженных за день") +
  geom_line() +
  geom_line(data = ar_train, mapping = aes(x = Date_reported, y = New_cases, colour = "Данные модели авторегрессии")) +
  ggtitle("Данные по количеству зараженных в день COVID 19 с 12.5.2020 по 15.5.2023")

cut_size = 30
df_cut = df[(length(df$New_cases)-cut_size):length(df$New_cases),]
ar_train_cut = ar_train[(length(ar_train$New_cases)-cut_size):length(ar_train$New_cases),]
plot1 = ggplot(df_cut, aes(x = Date_reported, y = New_cases, colour = "Исходные данные")) + xlab("Дата") + ylab("Количество зараженных за день") +
  geom_line() +
  geom_line(data = ar_train_cut, mapping = aes(x = Date_reported, y = New_cases, colour = "Данные модели авторегрессии")) +
  scale_x_date(labels = date_format("%d-%m"))

k = 1000

ar_train =  data.frame(New_cases = yt_train(df, k), Date_reported = df$Date_reported[k:length(df$Date_reported)]) 
ggplot(df, aes(x = Date_reported, y = New_cases, colour = "Исходные данные")) + xlab("Дата") + ylab("Количество зараженных за день") +
  geom_line() +
  geom_line(data = ar_train, mapping = aes(x = Date_reported, y = New_cases, colour = "Данные модели авторегрессии")) +
  ggtitle("Данные по количеству зараженных в день COVID 19 с 12.5.2020 по 15.5.2023")

cut_size = 30
df_cut = df[(length(df$New_cases)-cut_size):length(df$New_cases),]
ar_train_cut = ar_train[(length(ar_train$New_cases)-cut_size):length(ar_train$New_cases),]
plot2 = ggplot(df_cut, aes(x = Date_reported, y = New_cases, colour = "Исходные данные")) + xlab("Дата") + ylab("Количество зараженных за день") +
  geom_line() +
  geom_line(data = ar_train_cut, mapping = aes(x = Date_reported, y = New_cases, colour = "Данные модели авторегрессии")) +
  scale_x_date(labels = date_format("%d-%m"))

ggarrange(plot1, plot2, labels = c("k = 100", "k = 1000"))

yt_forecast = function(df, k, n) {
  yt_vec = c()
  for (i in 1:n) {
    if (i <= k) {
      window_vector = c(df$New_cases[(length(df$New_cases) - (k-i)):length(df$New_cases)], yt_vec)
    } else {
      window_vector = yt_vec[(length(yt_vec) - (k-1)):length(yt_vec)]
    }
    yt = ar_predicte_model(window_vector)
    yt_vec = c(yt_vec, yt)
  }
  return(yt_vec)
}

k = 18
n = 10

ar_forecast = data.frame(New_cases = c(df$New_cases[length(df$New_cases)], yt_forecast(df, k, n)), Date_reported = c(df$Date_reported[length(df$Date_reported)], seq.Date(df$Date_reported[length(df$Date_reported)] + 1, df$Date_reported[length(df$Date_reported)] + n, "day")))
ggplot(df, aes(Date_reported, New_cases)) + xlab("Дата") + ylab("Количество зараженных за день") +
  geom_line() +
  geom_line(data = ar_forecast, mapping = aes(Date_reported, New_cases), color = "red")
ggplot(df_cut, aes(Date_reported, New_cases)) + xlab("Дата") + ylab("Количество зараженных за день") +
  geom_line() +
  geom_line(data = ar_forecast, mapping = aes(Date_reported, New_cases), color = "red") +
  scale_x_date(labels = date_format("%d-%m-%Y"))
