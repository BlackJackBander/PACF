# ==========================================
# Скрипт анализа цикличности VTBR (R)
# ==========================================

# 1. Установка и подключение библиотек
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quantmod, forecast, TTR, ggplot2, lubridate, readr)

# 2. Загрузка данных
# Убедитесь, что файл находится в рабочей директории
file_path <- "C:/Users/admin/Downloads/SVCB_FULL_NOTES_2026-02-17.csv"
df <- read_csv(file_path, show_col_types = FALSE)

# Преобразование даты и сортировка
df$Date <- as.Date(df$Date)
df <- df[order(df$Date), ]

# Создание объекта xts (расширенный временной ряд) для тех. анализа
ts_data <- xts(df[,c("Open", "High", "Low", "Close", "Volume")], order.by = df$Date)

# 3. Декомпозиция временного ряда (Seasonality Trend Loess - STL)
# Используем частоту 21 (среднее число торговых дней в месяце)
close_ts <- ts(df$Close, frequency = 21) 
decomp <- stl(close_ts, s.window = "periodic")

# Вывод графика декомпозиции
plot(decomp, main = "Декомпозиция VTBR: Тренд, Сезонность и Остатки")

# 4. Технический анализ и поиск циклов
# Рассчитаем RSI и Стохастик для подтверждения перекупленности
rsi <- RSI(Cl(ts_data), n = 14)
stoch <- stoch(HLC(ts_data), nFastK = 14, nFastD = 3, nSlowD = 3)

# Построение графика с индикаторами
chartSeries(ts_data, 
            subset = "last 6 months",
            theme = chartTheme("white"), 
            TA = c(addBBands(), addRSI(), addMACD()),
            name = "VTBR: Technical Analysis & Cycles")

# 5. Анализ автокорреляции (ACF) для подтверждения цикла
acf(df$Close, lag.max = 50, main = "ACF Test: Поиск цикличности")

# 6. Вывод последних данных и сигнала
last_price <- as.numeric(last(ts_data$Close))
last_rsi <- as.numeric(last(rsi))
last_stoch_k <- as.numeric(last(stoch$fastK))

cat("========================================\n")
cat("ОТЧЕТ ПО ТИКЕРУ VTBR (Current State)\n")
cat("========================================\n")
cat("Дата анализа: ", as.character(last(index(ts_data))), "\n")
cat("Цена Close: ", last_price, "\n")
cat("RSI (14): ", round(last_rsi, 2), " ( >70 = Перекупленность)\n")
cat("Stoch K: ", round(last_stoch_k, 2), " ( >80 = Высокий риск разворота)\n")

if (last_stoch_k > 80) {
  cat("РЕКОМЕНДАЦИЯ: Внимание! Актив перегрет. Возможен конец цикла роста.\n")
} else {
  cat("РЕКОМЕНДАЦИЯ: Нейтральная зона или зона покупок.\n")
}

# ==========================================
# 7. Вывод результатов ACF в таблицу
# ==========================================

# Расчет ACF без построения графика (plot = FALSE)
# lag.max = 30 (смотрим месяц + запас)
acf_result <- acf(df$Close, lag.max = 30, plot = FALSE)

# Расчет порога значимости (тот самый "синий туннель")
# Формула: +/- 1.96 / корень из количества наблюдений (для 95% доверительного интервала)
n_obs <- length(df$Close)
confidence_threshold <- 1.96 / sqrt(n_obs)

# Создание Dataframe
acf_table <- data.frame(
  Lag = as.numeric(acf_result$lag),          # Количество дней назад
  Correlation = as.numeric(acf_result$acf)   # Сила связи (-1 до 1)
)

# Добавляем колонку: Значимо ли это? (TRUE = Реальный сигнал, FALSE = Шум)
acf_table$Is_Significant <- abs(acf_table$Correlation) > confidence_threshold

# Убираем Лаг 0 (он всегда равен 1, так как это корреляция с самим собой)
acf_table <- acf_table[acf_table$Lag > 0, ]

# Вывод всей таблицы
print(acf_table)

# --- ПРОВЕРКА ГИПОТЕЗЫ 21 ДНЯ ---
cat("\n--- ПРОВЕРКА ЦИКЛА (21 ДЕНЬ) ---\n")
lag_21_data <- acf_table[acf_table$Lag == 21, ]
print(lag_21_data)

if(lag_21_data$Is_Significant) {
  cat("РЕЗУЛЬТАТ: Цикл 21 день статистически ПОДТВЕРЖДЕН (выход за пределы шума).")
} else {
  cat("РЕЗУЛЬТАТ: Цикл 21 день находится в зоне шума (связь слабая).")
}

# Если хотите сохранить в CSV
write.csv(acf_table, "acf_results.csv", row.names = FALSE)
