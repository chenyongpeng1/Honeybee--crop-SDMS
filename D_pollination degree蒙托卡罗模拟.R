# 加载必要的库
library(dplyr)
library(ggplot2)

# 手动输入数据
data <- data.frame(
  crop = c("appicot", "apple", "blueberry", "buckwheat", "coffee", "cotton", 
           "cucumber", "eggplant", "monggo", "peach", "pear", "plum", 
           "pumpkin", "rapeseed", "sesame", "soybean", "sunflower", "watermelon"),
  SW = c(0.5, 0.8, 0.4, NA, NA, 0.3, NA, NA, NA, 0.2, 0.5, 0.5, NA, NA, NA, 0.1, 0.8, 0.4),
  Cald = c(0.7, 1, 1, NA, NA, 0.2, 0.9, NA, NA, 0.6, 0.7, 0.7, 0.9, 1, NA, NA, NA, NA),
  Asia_Liu = c(0.42, 0.76, NA, 0.41, NA, 0.43, 0.35, 0.4, NA, 0.49, 0.98, 0.3, NA, 0.76, 0.39, 0.1, 0.39, 0.49),
  Robinson_US = c(0.6, 0.9, NA, NA, NA, 0.2, 0.8, NA, NA, 0.5, 0.6, 0.6, 0.8, 0.9, NA, 0.05, 0.9, 0.6),
  K_Mean = c(0.65, 0.65, 0.65, 0.65, 0.25, 0.25, 0.65, 0.25, 0.65, 0.65, 0.65, 0.65, 0.95, 0.25, 0.25, 0.25, 0.95, 0.95)
)


# 定义蒙特卡罗模拟函数
monte_carlo_simulation <- function(values, n = 10000) {
  # 去掉 NA 值
  values <- na.omit(values)
  
  # 如果没有可用数据，返回 NA
  if (length(values) == 0) {
    return(data.frame(mean_D = NA, ci_low = NA, ci_high = NA))
  }
  
  # 随机采样
  simulated <- sample(values, size = n, replace = TRUE)
  
  # 计算均值和置信区间
  mean_D <- mean(simulated)
  ci_low <- quantile(simulated, 0.025)
  ci_high <- quantile(simulated, 0.975)
  
  return(data.frame(mean_D = mean_D, ci_low = ci_low, ci_high = ci_high))
}


# 按作物运行蒙特卡罗模拟
results <- data %>%
  rowwise() %>%
  mutate(
    simulation = list(monte_carlo_simulation(c(SW, Cald, Asia_Liu, Robinson_US), n = 10000))
  ) %>%
  unnest(simulation)  # 展开模拟结果

# 检查结果
print(results)


# 可视化
ggplot(results, aes(x = reorder(crop, mean_D), y = mean_D)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  labs(
    title = "Monte Carlo Simulation of Crop Dependence",
    x = "Crop",
    y = "Dependence Coefficient (Mean ± 95% CI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 加载必要的库
library(ggplot2)

# 创建数据框
data <- data.frame(
  crop = c("appicot", "apple", "blueberry", "buckwheat", "coffee", "cotton", 
           "cucumber", "eggplant", "monggo", "peach", "pear", "plum", 
           "pumpkin", "rapeseed", "sesame", "soybean", "sunflower", "watermelon"),
  Mean = c(0.56, 0.87, 0.7, 0.65, 0.25, 0.28, 0.68, 0.25, 0.65, 0.45, 0.7, 
           0.53, 0.85, 0.89, 0.25, 0.25, 0.7, 0.5),
  D_025 = c(0.42, 0.76, 0.4, 0.89, 0.39, 0.2, 0.35, 0.39, 0.89, 0.2, 0.5, 
            0.3, 0.8, 0.76, 0.39, 0.39, 0.3, 0.4),
  D_975 = c(0.7, 1.0, 1.0, 0.41, 0.11, 0.43, 0.9, 0.11, 0.41, 0.6, 0.98, 
            0.7, 0.9, 1.0, 0.11, 0.11, 0.9, 0.6)
)

# 绘制符合 Nature 风格的柱状图
p<-ggplot(data, aes(x = reorder(crop, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbar(aes(ymin = D_025, ymax = D_975), width = 0.2, color = "black") +
  labs(
    # title = "Crop Dependence Coefficients with 95% CI",
    x = "Crop",
    y = "Dependence Coefficient (Mean ± 95% CI)"
  ) +
  theme_minimal(base_size = 12) +  # 设置基础字体大小为 12
  theme(
    panel.grid = element_blank(),         # 去掉网格线
    axis.line = element_line(color = "black", size = 0.5), # 添加横纵轴线
    axis.ticks = element_line(color = "black", size = 0.5), # 添加刻度线
    axis.text = element_text(size = 12),  # 设置刻度文本字体大小
    axis.title = element_text(size = 12), # 设置轴标题字体大小
    plot.title = element_text(size = 12, hjust = 0.5), # 标题字体大小和居中
    axis.text.x = element_text(angle = 45, hjust = 1)   # 斜体 x 轴标签
  )

# 导出图表为 PNG 文件
ggsave("crop_dependence_plot.png", plot = p, width = 10, height = 6, dpi = 300,bg=
         'white')

# 导出图表为 PDF 文件
ggsave("crop_dependence_plot.pdf", plot = p, width = 10, height = 6)
