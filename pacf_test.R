# =========================================================
# АНАЛИЗ PACF (Частная автокорреляция) для VTBR
# =========================================================

# 1. Загрузка данных
file_name <- "C:/Users/admin/Downloads/VTBR_FULL_NOTES_2026-02-17.csv"

# Проверка существования файла
if (!file.exists(file_name)) {
  stop("Файл не найден! Убедитесь, что он в рабочей директории.")
}

df <- read.csv(file_name, stringsAsFactors = FALSE)

# 2. Подготовка данных
# Преобразуем дату и сортируем (на случай, если данные перемешаны)
df$Date <- as.Date(df$Date)
df <- df[order(df$Date), ]

# Берем цену закрытия (Close)
prices <- df$Close

# Удаляем пустые значения (NA), если они есть
prices <- na.omit(prices)

# 3. Расчет PACF
# lag.max = 30 (смотрим на месяц вперед, чтобы захватить 21-й день)
pacf_result <- pacf(prices, lag.max = 30, plot = FALSE)

# 4. Расчет порога значимости (Blue Zone)
# Формула: 1.96 / корень(N) для 95% доверительного интервала
n_obs <- length(prices)
threshold <- 1.96 / sqrt(n_obs)

# 5. Создание итоговой таблицы
pacf_table <- data.frame(
  Lag = as.numeric(pacf_result$lag),
  PACF_Value = as.numeric(pacf_result$acf)
)

# Определяем, значим ли сигнал (выходит ли за пределы шума)
pacf_table$Is_Significant <- abs(pacf_table$PACF_Value) > threshold

# Округляем для красоты
pacf_table$PACF_Value <- round(pacf_table$PACF_Value, 4)

# 6. Вывод результатов
cat("\n--- РЕЗУЛЬТАТЫ PACF ТЕСТА ---\n")
cat("Порог шума (Threshold): +/-", round(threshold, 4), "\n\n")

# Показываем первые 10 лагов (краткосрочная память)
print(head(pacf_table, 10))

# Показываем конкретно 21-й день (Ваш цикл)
cat("\n--- ПРОВЕРКА ЦИКЛА (21 ДЕНЬ) ---\n")
lag_21 <- pacf_table[pacf_table$Lag == 21, ]
print(lag_21)

# Интерпретация для консоли
if(lag_21$Is_Significant) {
  cat("\nВЫВОД: На 21-м дне есть НОВЫЙ импульс (драйвер роста).\n")
} else {
  cat("\nВЫВОД: На 21-м дне НЕТ нового импульса. Рост идет по инерции.\n")
}

# 7. Визуализация (График)
# Строим график, чтобы глазами увидеть "торчащие палки"
plot(pacf_result, main = "PACF: Поиск скрытых импульсов VTBR")
abline(h = c(threshold, -threshold), col = "blue", lty = 2) # Линии порога

# 8. Сохранение в файл (опционально)
write.csv(pacf_table, "vtbr_pacf_results.csv", row.names = FALSE)
cat("\nТаблица сохранена в файл: vtbr_pacf_results.csv")