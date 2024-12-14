# Cài đặt các gói cần thiết
install.packages(c("quantmod", "tseries", "PerformanceAnalytics", "PortfolioAnalytics", "ggplot2", "dplyr"))
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ggplot2)
library(dplyr)

# 1. Thu thập dữ liệu giá cổ phiếu của Vinamilk (VNM)
#Xử lý mising data
VNM.VN <- na.approx(VNM.VN)
MSN.VN <- na.approx(MSN.VN)
FPT.VN <- na.approx(FPT.VN)

# Dữ liệu giá cổ phiếu VNM từ Yahoo Finance
tickers <- c("VNM.VN", "MSN.VN", "FPT.VN")  # Thêm các cổ phiếu khác vào danh mục
getSymbols(tickers, src = "yahoo", from = "2019-01-01", to = Sys.Date())
library(zoo)
vnm_price <- Cl(VNM.VN)  # Chỉ lấy giá đóng cửa


#Head data
head(VNM.VN)
head(MSN.VN)
head(FPT.VN)

# 2. Tính toán lợi suất hàng ngày của cổ phiếu
vnm_returns <- dailyReturn(vnm_price)

# 3. Phân tích thống kê cơ bản
summary(vnm_returns)

# Vẽ đồ thị lợi suất hàng ngày
plot(vnm_returns, main="Lợi suất cổ phiếu Vinamilk (VNM)")

# 4. Phân tích rủi ro sử dụng VaR (Value at Risk)
VaR_95 <- VaR(vnm_returns, p = 0.95, method = "historical")
VaR_99 <- VaR(vnm_returns, p = 0.99, method = "historical")

# In kết quả VaR
print(paste("VaR 95%:", VaR_95))
print(paste("VaR 99%:", VaR_99))

# 5. Tạo danh mục đầu tư và tối ưu hóa
# Giả sử chúng ta có một danh mục đầu tư gồm Vinamilk và một số cổ phiếu khác

tickers <- c("VNM.VN", "MSN.VN", "FPT.VN")  # Thêm các cổ phiếu khác vào danh mục
getSymbols(tickers, src = "yahoo", from = "2019-01-01", to = Sys.Date())

# Tính lợi suất hàng ngày của các cổ phiếu trong danh mục
prices <- lapply(tickers, function(ticker) Cl(get(ticker)))
returns <- lapply(prices, dailyReturn)
returns_data <- do.call(cbind, returns)

# Tính toán ma trận hiệp phương sai của các cổ phiếu
cov_matrix <- cov(returns_data)

# Tối ưu hóa danh mục đầu tư
#portfolio <- optimize.portfolio(returns_data)
#optimal_weights <- portfolio$weights
portfolio <- portfolio.spec(assets = colnames(returns_data))

# Thêm các ràng buộc vào portfolio (ví dụ: tổng trọng số = 1 và chỉ đầu tư vào tài sản có giá trị dương)
portfolio <- add.constraint(portfolio, type = "full_investment")  # Tổng trọng số = 1
portfolio <- add.constraint(portfolio, type = "long_only")         # Không cho phép bán khống

# Thêm các mục tiêu tối ưu hóa vào portfolio (ví dụ: tối đa hóa lợi nhuận kỳ vọng và tối thiểu hóa rủi ro)
portfolio <- add.objective(portfolio, type = "return", name = "mean") # Tối đa hóa lợi nhuận kỳ vọng
portfolio <- add.objective(portfolio, type = "risk", name = "StdDev") # Tối thiểu hóa rủi ro (Độ lệch chuẩn)

# Tối ưu hóa danh mục đầu tư
# Cài đặt gói DEoptim
install.packages("DEoptim")

# Nạp gói DEoptim
library(DEoptim)

optimal_portfolio <- optimize.portfolio(returns_data, portfolio)

# Lấy trọng số tối ưu của các tài sản trong danh mục
optimal_weights <- optimal_portfolio$weights

# In kết quả trọng số tối ưu
print(optimal_weights)
# In kết quả tối ưu hóa danh mục
print("Tỷ trọng tối ưu của các cổ phiếu trong danh mục:")
print(optimal_weights)

# 6. Dự đoán lợi nhuận tương lai
# Sử dụng mô hình hồi quy tuyến tính đơn giản
model <- lm(vnm_returns ~ index(vnm_returns))  # Hồi quy lợi suất theo thời gian
summary(model)

# Dự đoán lợi suất tương lai
future_returns <- predict(model, newdata = data.frame(VNM.VN = rep(as.Date(Sys.Date()) + 1, 368)))


# In dự đoán lợi suất tương lai
print(paste("Dự đoán lợi suất tương lai:", future_returns))

# 7. Vẽ đồ thị kết quả tối ưu hóa
barplot(optimal_weights, main="Tỷ trọng tối ưu trong danh mục đầu tư", col=rainbow(length(optimal_weights)), names.arg=tickers)
