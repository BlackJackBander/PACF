# Установка пакета pracma, если он еще не установлен
# install.packages("pracma")

library(pracma)

# 1. Загрузка данных
# Убедись, что имя файла соответствует тому, с которым работаешь
file_name <- 'ACTIVE_2026-02-26.csv'
df <- read.csv(file_name)

# Преобразование столбца Date в формат дат и сортировка по времени
df$Date <- as.Date(df$Date)
df <- df[order(df$Date), ]

# 2. Фильтрация данных (фокус на последние 12 месяцев)
last_year <- subset(df, Date >= as.Date('2025-02-01'))

# Извлекаем цены закрытия и соответствующие им даты в виде векторов
close_prices <- last_year$Close
dates <- last_year$Date

# 3. Поиск пиков (highs) и впадин (lows)
# Функция findpeaks возвращает матрицу. 
# 2-й столбец этой матрицы содержит ИНДЕКСЫ найденных экстремумов.
# minpeakdistance = 15 — аналог distance=15 из Python
peaks_matrix <- findpeaks(close_prices, minpeakdistance = 15)
troughs_matrix <- findpeaks(-close_prices, minpeakdistance = 15)

# Извлекаем индексы из 2-го столбца матриц
peak_indices <- peaks_matrix[, 2]
trough_indices <- troughs_matrix[, 2]

# Получаем даты по найденным индексам
peak_dates <- dates[peak_indices]
trough_dates <- dates[trough_indices]

# 4. Вычисление промежутков между пиками (в торговых днях)
peak_diffs <- diff(peak_indices)
avg_cycle <- ifelse(length(peak_diffs) > 0, mean(peak_diffs), 20)

# 5. Вывод результатов в консоль (аналог print)
cat("=== Аналог вывода Python ===\n")
cat("Recent Peaks:\n")
print(tail(peak_dates, 5))

cat("\nRecent Troughs:\n")
print(tail(trough_dates, 5))

cat("\nAverage cycle length (days):", round(avg_cycle, 2), "\n")
cat("Last Trough Date:", as.character(tail(trough_dates, 1)), "\n")
cat("Current Date:", as.character(tail(dates, 1)), "\n")
