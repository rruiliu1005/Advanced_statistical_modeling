# One-dimensional and multi-dimensional kernel regression.
library(np)
library(ggplot2)
library(GGally)
library(plotly)
library(tidyr)
library(dplyr)
library(gridExtra)


external_df = read.csv('external_df.csv')
external_df = external_df[-which(external_df$Oil<0),]
external_df_unchanged = external_df
external_df = data.frame(scale(external_df[,2:ncol(external_df)]))
external_df$Date = external_df_unchanged$Date
variables = c("VIX", "Oil", "Gold",  "SP500", "Nikkei", "SSE", "FTSE")
external_df = external_df[, c("Date", "VIX", "Oil", "Gold",  "SP500", "Nikkei", "SSE", "FTSE")]
external_df_original = external_df
set.seed(999)
training_index = sample(1:nrow(external_df), 1600)
external_df = external_df_original[training_index, ]
external_df_test = external_df_original[-training_index, ]


pairwise_data = external_df[, c("VIX", "Oil", "Gold", "SP500", "Nikkei", "SSE", "FTSE")]
plots = list()
for (i in 1:ncol(pairwise_data)) {
  col_name = colnames(pairwise_data)[i]
  plots[[i]] = ggplot(data = pairwise_data, aes_string(x = col_name, y = "..density..")) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = "", x = col_name, y = "Density") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#FFC8A2", color = NA),  
      plot.background = element_rect(fill = "#FFC8A2", color = NA),   
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#FFC8A2", color = NA), 
      strip.text = element_text(color = "black"), 
      axis.text = element_text(color = "black", size = 15),   
      axis.title = element_text(color = "black"),  
      plot.title = element_text(color = "black"), 
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)   
    )
}

grid_plot = grid.arrange(grobs = plots, ncol = 4, nrow = 2)
ggsave("histogram.jpeg", grid_plot, bg = "#FFC8A2", width = 13, height = 8)

plots = list()
for (i in 2:ncol(pairwise_data)) {
  col_name = colnames(pairwise_data)[i]
  plots[[i-1]] = ggplot(data = pairwise_data, aes_string(x = col_name, y = "VIX")) +
    geom_point(color = "skyblue") + 
    labs(title = "", x = col_name, y = "VIX") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#FFC8A2", color = NA),  
      plot.background = element_rect(fill = "#FFC8A2", color = NA),   
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#FFC8A2", color = NA), 
      strip.text = element_text(color = "black"), 
      axis.text = element_text(color = "black", size = 15),   
      axis.title = element_text(color = "black"),  
      plot.title = element_text(color = "black"), 
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )
}
grid_plot = grid.arrange(grobs = plots, ncol = 3, nrow = 2)
ggsave("scatter_plot.jpeg", grid_plot, bg = "#FFC8A2", width = 12, height = 8)


pairwise_data = external_df[, c("VIX", "Oil", "Gold", "SP500", "Nikkei", "SSE", "FTSE")]
pairwise_plot = ggpairs(pairwise_data)
ggsave("pairwise_plot.jpeg", plot = pairwise_plot, width = 12, height = 8, dpi = 300)


scaled_data = log(pairwise_data)
plots = list()
for (i in 2:ncol(scaled_data)) {
  col_name = colnames(scaled_data)[i]
  plots[[i-1]] = ggplot(data = scaled_data, aes_string(x = col_name, y = "VIX")) +
    geom_point(color = "skyblue") + 
    labs(title = "", x = col_name, y = "VIX") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#FFC8A2", color = NA),  
      plot.background = element_rect(fill = "#FFC8A2", color = NA),   
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#FFC8A2", color = NA), 
      strip.text = element_text(color = "black"), 
      axis.text = element_text(color = "black", size = 15),   
      axis.title = element_text(color = "black"),  
      plot.title = element_text(color = "black")   
    )
}

grid_plot = grid.arrange(grobs = plots, ncol = 3, nrow = 2)
ggsave("scatter_plot_log_transformation.jpeg", grid_plot, bg = "#FFC8A2", width = 12, height = 8)

scaled_data = scale(pairwise_data)
plots = list()
for (i in 2:ncol(scaled_data)) {
  col_name = colnames(scaled_data)[i]
  plots[[i-1]] = ggplot(data = scaled_data, aes_string(x = col_name, y = "VIX")) +
    geom_point(color = "skyblue") + 
    labs(title = "", x = col_name, y = "VIX") +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#F1EDE6", color = NA),  
      plot.background = element_rect(fill = "#F1EDE6", color = NA),   
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#F1EDE6", color = NA), 
      strip.text = element_text(color = "black"), 
      axis.text = element_text(color = "black", size = 15),   
      axis.title = element_text(color = "black"),  
      plot.title = element_text(color = "black"), 
      axis.text.x = element_text(size = 10), 
      axis.text.y = element_text(size = 10)
    )
}

grid_plot = grid.arrange(grobs = plots, ncol = 3, nrow = 2)
ggsave("scatter_plot_scaled.jpeg", grid_plot, bg = "#F1EDE6", width = 12, height = 8)


# set.seed(888)
# testing_set = sample(1:nrow(external_df), 0.1*nrow(external_df))
# training = external_df[-testing_set, ]
# testing =  external_df[testing_set, ]
plots = list()

##################################################
#Kernel Choices 
##################################################
plot_x = seq(-3,3, by = 0.01)
gaussian_kernel = dnorm(plot_x)
plot_data = data.frame(x = plot_x, gaussian = gaussian_kernel)
p = ggplot(data = plot_data, aes(x = plot_x, y = gaussian))+geom_line()+theme_minimal() + 
  labs(title = "", x = "X", y = "PDF") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#C6DBDA", color = NA),  
    plot.background = element_rect(fill = "#C6DBDA", color = NA),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#C6DBDA", color = NA), 
    strip.text = element_text(color = "black"), 
    axis.text = element_text(color = "black", size = 15),   
    axis.title = element_text(color = "black"),  
    plot.title = element_text(color = "black")   
  )
ggsave("gaussian_kernel.jpeg", p, bg = "#C6DBDA", width = 8, height = 8)
plot_x = seq(-3,3, by = 0.01)
epanechnikov_kernel = 3/4*(1-plot_x^2)*as.numeric(abs(plot_x) <= 1)
plot_data = data.frame(x = plot_x, epanechnikov = epanechnikov_kernel)
p = ggplot(data = plot_data, aes(x = plot_x, y = epanechnikov))+geom_line()+theme_minimal() + 
  labs(title = "", x = "X", y = "PDF") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#C6DBDA", color = NA),  
    plot.background = element_rect(fill = "#C6DBDA", color = NA),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#C6DBDA", color = NA), 
    strip.text = element_text(color = "black"), 
    axis.text = element_text(color = "black", size = 15),   
    axis.title = element_text(color = "black"),  
    plot.title = element_text(color = "black")   
  )
ggsave("epanechnikov_kernel.jpeg", p, bg = "#C6DBDA", width = 8, height = 8)
plot_x = seq(-3,3, by = 0.01)
uniform_kernel = 1/2*as.numeric(abs(plot_x) <= 1)
plot_data = data.frame(x = plot_x, uniform = uniform_kernel)
p = ggplot(data = plot_data, aes(x = plot_x, y = uniform))+geom_line()+theme_minimal() + 
  labs(title = "", x = "X", y = "PDF") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#C6DBDA", color = NA),  
    plot.background = element_rect(fill = "#C6DBDA", color = NA),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#C6DBDA", color = NA), 
    strip.text = element_text(color = "black"), 
    axis.text = element_text(color = "black", size = 15),   
    axis.title = element_text(color = "black"),  
    plot.title = element_text(color = "black")   
  )
ggsave("uniform_kernel.jpeg", p, bg = "#C6DBDA", width = 8, height = 8)

# external_df_original = external_df
# external_df_testing = external_df[external_df$Date >= '2024-09-01', ]
# external_df = external_df[(external_df$Date < '2024-09-01'), ]
##################################################
# One-Dimensional Kernel regression - local constant 
##################################################
# regression_df = scale(external_df[, c('Oil', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ Oil, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Oil = regression_df$Oil, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[1]] = ggplot(plot_data, aes(x = Oil, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = scale(external_df[, c('Gold', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ Gold, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Gold = regression_df$Gold, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[2]] = ggplot(plot_data, aes(x = Gold, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# ##################################################
# regression_df = scale(external_df[, c('SP500', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ SP500, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SP500 = regression_df$SP500, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[3]] = ggplot(plot_data, aes(x = SP500, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# 
# ##################################################
# regression_df = scale(external_df[, c('Nikkei', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ Nikkei, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Nikkei = regression_df$Nikkei, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[4]] = ggplot(plot_data, aes(x = Nikkei, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = scale(external_df[, c('SSE', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[5]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = scale(external_df[, c('FTSE', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(FTSE = regression_df$FTSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[6]] = ggplot(plot_data, aes(x = FTSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# grid_plot = grid.arrange(grobs = plots, ncol = 2, nrow = 3)
# ggsave("one_dimensional_kernel_regression_lc_epanechnikov.jpeg", plot = grid_plot, width = 12, height = 10, dpi = 300)
# 
# 
# ##################################################
# # One-Dimensional Kernel regression - Local Linear, epanechnikov 
# #######################################################
# regression_df = scale(external_df[, c('Oil', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ Oil, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Oil = regression_df$Oil, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# 
# 
# plots[[1]] = ggplot(plot_data, aes(x = Oil, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = scale(external_df[, c('Gold', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ Gold, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Gold = regression_df$Gold, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# 
# 
# plots[[2]] = ggplot(plot_data, aes(x = Gold, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = scale(external_df[, c('SP500', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ SP500, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SP500 = regression_df$SP500, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# 
# plots[[3]] = ggplot(plot_data, aes(x = SP500, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# 
# ##################################################
# regression_df = scale(external_df[, c('Nikkei', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ Nikkei, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Nikkei = regression_df$Nikkei, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[4]] = ggplot(plot_data, aes(x = Nikkei, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = scale(external_df[, c('SSE', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[5]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = scale(external_df[, c('FTSE', 'VIX')])
# regression_df = data.frame(regression_df)
# model = npreg(VIX ~ FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(FTSE = regression_df$FTSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[6]] = ggplot(plot_data, aes(x = FTSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     panel.background = element_rect(fill = "#F1EDE6", color = NA),
#     plot.background = element_rect(fill = "#F1EDE6", color = NA),
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# grid_plot = grid.arrange(grobs = plots, ncol = 2, nrow = 3)
# ggsave("one_dimensional_kernel_regression_ll_epanechnikov.jpeg", plot = grid_plot, width = 12, height = 10, dpi = 300)
# 
# ##################################################
# # One-Dimensional Kernel regression - local constant Gaussian
# ##################################################
# regression_df = external_df[, c('SP500', 'VIX')]
# model = npreg(VIX ~ SP500, data = regression_df, ckertype = "gaussian", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SP500 = regression_df$SP500, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[1]] = ggplot(plot_data, aes(x = SP500, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "gaussian", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[2]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('Gold', 'VIX')]
# model = npreg(VIX ~ Gold, data = regression_df, ckertype = "gaussian", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Gold = regression_df$Gold, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[3]] = ggplot(plot_data, aes(x = Gold, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# 
# ##################################################
# regression_df = external_df[, c('Nikkei', 'VIX')]
# model = npreg(VIX ~ Nikkei, data = regression_df, ckertype = "gaussian", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Nikkei = regression_df$Nikkei, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[4]] = ggplot(plot_data, aes(x = Nikkei, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "gaussian", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[5]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('FTSE', 'VIX')]
# model = npreg(VIX ~ FTSE, data = regression_df, ckertype = "gaussian", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(FTSE = regression_df$FTSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[6]] = ggplot(plot_data, aes(x = FTSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# grid_plot = grid.arrange(grobs = plots, ncol = 2, nrow = 3)
# ggsave("one_dimensional_kernel_regression_lc_gaussian.jpeg", plot = grid_plot, width = 12, height = 10, dpi = 300)
# 
# 
# ##################################################
# # One-Dimensional Kernel regression - Local Polynomial, gaussian 
# #######################################################
# regression_df = external_df[, c('SP500', 'VIX')]
# model = npreg(VIX ~ SP500, data = regression_df, ckertype = "gaussian", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SP500 = regression_df$SP500, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[1]] = ggplot(plot_data, aes(x = SP500, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "gaussian", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[2]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('Gold', 'VIX')]
# model = npreg(VIX ~ Gold, data = regression_df, ckertype = "gaussian", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Gold = regression_df$Gold, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[3]] = ggplot(plot_data, aes(x = Gold, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# 
# ##################################################
# regression_df = external_df[, c('Nikkei', 'VIX')]
# model = npreg(VIX ~ Nikkei, data = regression_df, ckertype = "gaussian", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Nikkei = regression_df$Nikkei, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[4]] = ggplot(plot_data, aes(x = Nikkei, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "gaussian", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[5]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('FTSE', 'VIX')]
# model = npreg(VIX ~ FTSE, data = regression_df, ckertype = "gaussian", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(FTSE = regression_df$FTSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[6]] = ggplot(plot_data, aes(x = FTSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# grid_plot = grid.arrange(grobs = plots, ncol = 2, nrow = 3)
# ggsave("one_dimensional_kernel_regression_ll_gaussian.jpeg", plot = grid_plot, width = 12, height = 10, dpi = 300)
# 
# 
# ##################################################
# # One-Dimensional Kernel regression - local constant 
# ##################################################
# regression_df = external_df[, c('SP500', 'VIX')]
# model = npreg(VIX ~ SP500, data = regression_df, ckertype = "uniform", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SP500 = regression_df$SP500, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[1]] = ggplot(plot_data, aes(x = SP500, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "uniform", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[2]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('Gold', 'VIX')]
# model = npreg(VIX ~ Gold, data = regression_df, ckertype = "uniform", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Gold = regression_df$Gold, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[3]] = ggplot(plot_data, aes(x = Gold, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# 
# ##################################################
# regression_df = external_df[, c('Nikkei', 'VIX')]
# model = npreg(VIX ~ Nikkei, data = regression_df, ckertype = "uniform", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Nikkei = regression_df$Nikkei, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[4]] = ggplot(plot_data, aes(x = Nikkei, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "uniform", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[5]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('FTSE', 'VIX')]
# model = npreg(VIX ~ FTSE, data = regression_df, ckertype = "uniform", regtype = 'lc')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(FTSE = regression_df$FTSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[6]] = ggplot(plot_data, aes(x = FTSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "#8FCACA", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# grid_plot = grid.arrange(grobs = plots, ncol = 2, nrow = 3)
# ggsave("one_dimensional_kernel_regression_lc_uniform.jpeg", plot = grid_plot, width = 12, height = 10, dpi = 300)
# 

##################################################
# One-Dimensional Kernel regression - Local Polynomial, uniform 
#######################################################
# regression_df = external_df[, c('SP500', 'VIX')]
# model = npreg(VIX ~ SP500, data = regression_df, ckertype = "uniform", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SP500 = regression_df$SP500, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[1]] = ggplot(plot_data, aes(x = SP500, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "uniform", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[2]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('Gold', 'VIX')]
# model = npreg(VIX ~ Gold, data = regression_df, ckertype = "uniform", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Gold = regression_df$Gold, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[3]] = ggplot(plot_data, aes(x = Gold, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# 
# ##################################################
# regression_df = external_df[, c('Nikkei', 'VIX')]
# model = npreg(VIX ~ Nikkei, data = regression_df, ckertype = "uniform", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(Nikkei = regression_df$Nikkei, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[4]] = ggplot(plot_data, aes(x = Nikkei, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('SSE', 'VIX')]
# model = npreg(VIX ~ SSE, data = regression_df, ckertype = "uniform", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(SSE = regression_df$SSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[5]] = ggplot(plot_data, aes(x = SSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# ##################################################
# regression_df = external_df[, c('FTSE', 'VIX')]
# model = npreg(VIX ~ FTSE, data = regression_df, ckertype = "uniform", regtype = 'll')
# summary(model)
# regression_df$Type = "Observed Data"
# fitted_data = data.frame(FTSE = regression_df$FTSE, VIX = fitted(model), Type = "Fitted Regression Line")
# 
# plot_data = rbind(
#   regression_df, 
#   fitted_data
# )
# 
# plots[[6]] = ggplot(plot_data, aes(x = FTSE, y = VIX, color = Type)) +
#   geom_point(data = subset(plot_data, Type == "Observed Data"), size = 1, alpha = 0.7) +  
#   geom_line(data = subset(plot_data, Type == "Fitted Regression Line"), size = 1) + 
#   theme_minimal() +
#   scale_color_manual(values = c("Observed Data" = "pink", "Fitted Regression Line" = "black")) +
#   labs(color = "Legend") + 
#   theme(
#     legend.position = c(0.8, 0.8),  
#     legend.background = element_rect(fill = "white", color = "black", size = 0.5), 
#     legend.title = element_text(size = 10),  
#     legend.text = element_text(size = 8)   
#   )
# 
# grid_plot = grid.arrange(grobs = plots, ncol = 2, nrow = 3)
# ggsave("one_dimensional_kernel_regression_ll_uniform.jpeg", plot = grid_plot, width = 12, height = 10, dpi = 300)

######################################################################################################################################################
# Multi-Dimensional Kernel regression
######################################################################################################################################################
#Full Model
regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
full_model_lc = npreg(VIX ~ Oil + Gold + SP500 + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(full_model_lc)
covariate_range = apply(regression_df[, 2:(ncol(regression_df))], 2, max) - apply(regression_df[, 2:(ncol(regression_df))], 2, min)
round(full_model_lc$bw/covariate_range, 2)


regression_df$fitted_values = fitted(full_model_lc)
long_df = regression_df %>%
  pivot_longer(cols = SP500:FTSE, names_to = "Predictor", values_to = "Value")


 # p = ggplot(long_df, aes(x = Value, y = VIX)) +
 #  geom_point(color = "pink", alpha = 0.6, cex = 0.5) +
 #  geom_line(aes(y = fitted_values), color = "grey", size = 0.5) +
 #  facet_wrap(~ Predictor, scales = "free_x", ncol = 2) +
 #  labs(title = "VIX vs Predictors with Fitted Values",
 #       x = "Predictor Value", y = "VIX") +
 #  theme_minimal()
 # ggsave("multi_dimensional_kernel_regression_lc_full.jpeg", plot = p, width = 12, height = 16, dpi = 300)
 
regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
covariate_range = apply(regression_df[, 2:(ncol(regression_df))], 2, max) - apply(regression_df[, 2:(ncol(regression_df))], 2, min)
full_model_ll = npreg(VIX ~ Oil + Gold + SP500 + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(full_model_ll)
round(full_model_ll$bw/covariate_range, 2)
regression_df$fitted_values = fitted(full_model_ll)
long_df = regression_df %>%
  pivot_longer(cols = Oil:FTSE, names_to = "Predictor", values_to = "Value")
p = ggplot(long_df, aes(x = Value, y = VIX)) +
  geom_point(color = "pink", alpha = 0.6, cex = 0.5) +
  geom_line(aes(y = fitted_values), color = "grey", size = 0.5) +
  facet_wrap(~ Predictor, scales = "free_x", ncol = 2) +
  labs(title = "VIX vs Predictors with Fitted Values",
       x = "Predictor Value", y = "VIX") +
  theme_minimal()
ggsave("multi_dimensional_kernel_regression_ll_full.jpeg", plot = p, width = 16, height = 10, dpi = 300)


#plot_df = data.frame(scale(external_df_original[external_df_original$Date>=min(external_df_test$Date), variables]))
plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(full_model_lc, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(full_model_lc, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(full_model_lc$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
#ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_full_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(full_model_ll, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(full_model_ll, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(full_model_ll$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
#ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_full_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = regression_df
plot_df$Date = external_df$Date
long_df <- plot_df %>%
  pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value")
long_df$Date = as.Date(long_df$Date)
p <- ggplot(long_df, aes(x = Date, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "", x = "Date", y = "Value", color = "Variable") + 
  scale_color_manual(values = c("VIX" = "skyblue", "SP500" = "pink", "Gold" = "#9AD7A4", 
                                "SP500" = "#C59A7B", "Nikkei" = "#C274A8", "SSE" = "#DA8541", 
                                "FTSE" = "#B1A7FF")) + 
  scale_x_date(date_labels = "%Y-%m", date_breaks = "12 month") + 
  theme(
    #legend.position = c(0.8, 0.85), 
    legend.background = element_rect(fill = alpha("#FFC8A2", 0.8), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"), 
    axis.text.y = element_text(size = 30), 
    panel.background = element_rect(fill = "#FFC8A2", color = NA),
    plot.background = element_rect(fill = "#FFC8A2", color = NA),
    axis.text.x = element_text(size = 30), 
    axis.text = element_text(color = "black", size = 30), 
    axis.title = element_text(color = "black", size = 30)
  ) +
  labs(color = "Variable") 

ggsave("scaled_data_time_series.jpeg", plot = p, width = 45, height = 10, dpi = 300)


#########################################################################################################################################
#five variables 
regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_lc_oil = npreg(VIX ~ Gold + SP500 + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(five_model_lc_oil)
covariate_range = apply(regression_df[, five_model_lc_oil$xnames], 2, max) - apply(regression_df[, five_model_lc_oil$xnames], 2, min)
round(five_model_lc_oil$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_lc_gold = npreg(VIX ~ Oil + SP500 + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(five_model_lc_gold)
covariate_range = apply(regression_df[, five_model_lc_gold$xnames], 2, max) - apply(regression_df[, five_model_lc_gold$xnames], 2, min)
round(five_model_lc_gold$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_lc_SP500 = npreg(VIX ~ Oil + Gold + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
covariate_range = apply(regression_df[, five_model_lc_SP500$xnames], 2, max) - apply(regression_df[, five_model_lc_SP500$xnames], 2, min)
summary(five_model_lc_SP500)
round(five_model_lc_SP500$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_lc_Nikkei = npreg(VIX ~ Oil + Gold + SP500 + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
covariate_range = apply(regression_df[, five_model_lc_Nikkei$xnames], 2, max) - apply(regression_df[, five_model_lc_Nikkei$xnames], 2, min)
summary(five_model_lc_Nikkei)
round(five_model_lc_Nikkei$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_lc_SSE = npreg(VIX ~ Oil + Gold + SP500 + Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
covariate_range = apply(regression_df[, five_model_lc_SSE$xnames], 2, max) - apply(regression_df[, five_model_lc_SSE$xnames], 2, min)
summary(five_model_lc_SSE)
round(five_model_lc_SSE$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_lc_FTSE = npreg(VIX ~ Oil + Gold + SP500 + SSE + Nikkei, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
covariate_range = apply(regression_df[, five_model_lc_FTSE$xnames], 2, max) - apply(regression_df[, five_model_lc_FTSE$xnames], 2, min)
summary(five_model_lc_FTSE)
round(five_model_lc_FTSE$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_ll_oil = npreg(VIX ~ Gold + SP500 + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(five_model_ll_oil)
covariate_range = apply(regression_df[, five_model_ll_oil$xnames], 2, max) - apply(regression_df[, five_model_ll_oil$xnames], 2, min)
round(five_model_ll_oil$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_ll_SP500 = npreg(VIX ~ Gold + SP500 + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(five_model_ll_SP500)
covariate_range = apply(regression_df[, five_model_ll_SP500$xnames], 2, max) - apply(regression_df[, five_model_ll_SP500$xnames], 2, min)
round(five_model_ll_SP500$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_ll_gold = npreg(VIX ~ Oil + SP500 + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(five_model_ll_gold)
covariate_range = apply(regression_df[, five_model_ll_gold$xnames], 2, max) - apply(regression_df[, five_model_ll_gold$xnames], 2, min)
round(five_model_ll_gold$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_ll_SP500 = npreg(VIX ~ Oil + Gold + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
covariate_range = apply(regression_df[, five_model_ll_SP500$xnames], 2, max) - apply(regression_df[, five_model_ll_SP500$xnames], 2, min)
summary(five_model_ll_SP500)
round(five_model_ll_SP500$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_ll_Nikkei = npreg(VIX ~ Oil + Gold + SP500 + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
covariate_range = apply(regression_df[, five_model_ll_Nikkei$xnames], 2, max) - apply(regression_df[, five_model_ll_Nikkei$xnames], 2, min)
summary(five_model_ll_Nikkei)
round(five_model_ll_Nikkei$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_ll_SSE = npreg(VIX ~ Oil + Gold + SP500 + Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
covariate_range = apply(regression_df[, five_model_ll_SSE$xnames], 2, max) - apply(regression_df[, five_model_ll_SSE$xnames], 2, min)
summary(five_model_ll_SSE)
round(five_model_ll_SSE$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
five_model_ll_FTSE = npreg(VIX ~ Oil + Gold + SP500 + SSE + Nikkei, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
covariate_range = apply(regression_df[, five_model_ll_FTSE$xnames], 2, max) - apply(regression_df[, five_model_ll_FTSE$xnames], 2, min)
summary(five_model_ll_FTSE)
round(five_model_ll_FTSE$bw/covariate_range, 2)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_lc_oil, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_lc_oil, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_lc_oil$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_five_oil_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_lc_gold, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_lc_gold, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_lc_gold$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_five_gold_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_lc_SP500, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_lc_SP500, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_lc_SP500$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_five_SP500_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_lc_Nikkei, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_lc_Nikkei, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_lc_Nikkei$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_five_Nikkei_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_lc_SSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_lc_SSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_lc_SSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_five_SSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_lc_FTSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_lc_FTSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_lc_FTSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_five_FTSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)


plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_ll_oil, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_ll_oil, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_ll_oil$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_five_oil_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_ll_gold, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_ll_gold, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_ll_gold$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_five_gold_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_ll_SP500, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_ll_SP500, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_ll_SP500$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_five_SP500_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_ll_Nikkei, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_ll_Nikkei, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_ll_Nikkei$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_five_Nikkei_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_ll_SSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_ll_SSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_ll_SSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_five_SSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(five_model_ll_FTSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(five_model_ll_FTSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(five_model_ll_FTSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_five_FTSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)


#########################################################################################################################################
#four variables 
regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_lc_oil = npreg(VIX ~ Gold + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(four_model_lc_oil)
covariate_range = apply(regression_df[, four_model_lc_oil$xnames], 2, max) - apply(regression_df[, four_model_lc_oil$xnames], 2, min)
round(four_model_lc_oil$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_lc_gold = npreg(VIX ~ Oil + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(four_model_lc_gold)
covariate_range = apply(regression_df[, four_model_lc_gold$xnames], 2, max) - apply(regression_df[, four_model_lc_gold$xnames], 2, min)
round(four_model_lc_gold$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_lc_Nikkei = npreg(VIX ~ Oil + Gold + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(four_model_lc_Nikkei)
covariate_range = apply(regression_df[, four_model_lc_Nikkei$xnames], 2, max) - apply(regression_df[, four_model_lc_Nikkei$xnames], 2, min)
round(four_model_lc_Nikkei$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_lc_SSE = npreg(VIX ~ Oil + Gold + Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(four_model_lc_SSE)
covariate_range = apply(regression_df[, four_model_lc_SSE$xnames], 2, max) - apply(regression_df[, four_model_lc_SSE$xnames], 2, min)
round(four_model_lc_SSE$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_lc_FTSE = npreg(VIX ~ Oil + Gold + Nikkei + SSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(four_model_lc_FTSE)
covariate_range = apply(regression_df[, four_model_lc_FTSE$xnames], 2, max) - apply(regression_df[, four_model_lc_FTSE$xnames], 2, min)
round(four_model_lc_FTSE$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_ll_gold = npreg(VIX ~ SP500+ Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(four_model_ll_gold)
covariate_range = apply(regression_df[, four_model_ll_gold$xnames], 2, max) - apply(regression_df[, four_model_ll_gold$xnames], 2, min)
round(four_model_ll_gold$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_ll_SP500 = npreg(VIX ~ Gold + Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(four_model_ll_SP500)
covariate_range = apply(regression_df[, four_model_ll_SP500$xnames], 2, max) - apply(regression_df[, four_model_ll_SP500$xnames], 2, min)
round(four_model_ll_SP500$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_ll_Nikkei = npreg(VIX ~ Gold +SP500 + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(four_model_ll_Nikkei)
covariate_range = apply(regression_df[, four_model_ll_Nikkei$xnames], 2, max) - apply(regression_df[, four_model_ll_Nikkei$xnames], 2, min)
round(four_model_ll_Nikkei$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_ll_SSE = npreg(VIX ~ Gold +SP500+ Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(four_model_ll_SSE)
covariate_range = apply(regression_df[, four_model_ll_SSE$xnames], 2, max) - apply(regression_df[, four_model_ll_SSE$xnames], 2, min)
round(four_model_ll_SSE$bw/covariate_range, 2)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
four_model_ll_FTSE = npreg(VIX ~ Gold +SP500+ Nikkei + SSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(four_model_ll_FTSE)
covariate_range = apply(regression_df[, four_model_ll_FTSE$xnames], 2, max) - apply(regression_df[, four_model_ll_FTSE$xnames], 2, min)
round(four_model_ll_FTSE$bw/covariate_range, 2)


plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_lc_oil, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_lc_oil, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_lc_oil$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_four_oil_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_lc_gold, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_lc_gold, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_lc_gold$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_four_gold_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_lc_Nikkei, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_lc_Nikkei, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_lc_Nikkei$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_four_Nikkei_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_lc_SSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_lc_SSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_lc_SSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_four_SSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_lc_FTSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_lc_FTSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_lc_FTSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_lc_four_FTSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)


tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_ll_gold, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_ll_gold, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_ll_gold$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_four_gold_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_ll_SP500, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_ll_SP500, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_ll_SP500$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_four_SP500_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_ll_Nikkei, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_ll_Nikkei, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_ll_Nikkei$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_four_Nikkei_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_ll_SSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_ll_SSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_ll_SSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_four_SSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)

plot_df = data.frame(external_df[, variables])
colnames(plot_df) = variables
#plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_test$Date),c("Date")]
plot_df$Date = external_df$Date
plot_df$type = "Observed"

tmp1= data.frame(scale((external_df[, variables[2:length(variables)]])))
tmp1$VIX = predict(four_model_ll_FTSE, newdata = tmp1)
print(mean((external_df$VIX - tmp1$VIX)^2))
tmp = data.frame(scale((external_df_test[, variables[2:length(variables)]])))
tmp$VIX = predict(four_model_ll_FTSE, newdata = tmp)
print(mean((external_df_test$VIX - tmp$VIX)^2))
tmp$type = 'Fitted'
tmp$Date = external_df_test$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
print(four_model_ll_FTSE$R2)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 1) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 15),       
    legend.title = element_text(size = 15),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 15), 
    axis.text = element_text(color = "black", size = 15), 
    axis.title = element_text(color = "black", size = 15)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "12 months")
ggsave("ss.jpeg", plot = p, width = 25, height = 10, dpi = 300)
ggsave("kernel_regression_ll_four_FTSE_testing.jpeg", plot = p, width = 25, height = 5, dpi = 300)


#########################################################################################################################################
#three variables 
regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_lc_oil = npreg(VIX ~ Nikkei + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(three_model_lc_oil)
covariate_range = apply(regression_df[, three_model_lc_oil$xnames], 2, max) - apply(regression_df[, three_model_lc_oil$xnames], 2, min)
round(three_model_lc_oil$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_Nikkei_SP500 = npreg(Oil + SSE + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(three_model_Nikkei_SP500)
covariate_range = apply(regression_df[, three_model_Nikkei_SP500$xnames], 2, max) - apply(regression_df[, three_model_Nikkei_SP500$xnames], 2, min)
round(three_model_Nikkei_SP500$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_lc_SSE = npreg(VIX ~ Oil + Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(three_model_lc_SSE)
covariate_range = apply(regression_df[, three_model_lc_SSE$xnames], 2, max) - apply(regression_df[, three_model_lc_SSE$xnames], 2, min)
round(three_model_lc_SSE$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_lc_FTSE = npreg(VIX ~ Oil + Nikkei + SSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(three_model_lc_FTSE)
covariate_range = apply(regression_df[, three_model_lc_FTSE$xnames], 2, max) - apply(regression_df[, three_model_lc_FTSE$xnames], 2, min)
round(three_model_lc_FTSE$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_ll_gold = npreg(VIX ~ SP500 + Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(three_model_ll_gold)
covariate_range = apply(regression_df[, three_model_ll_gold$xnames], 2, max) - apply(regression_df[, three_model_ll_gold$xnames], 2, min)
round(three_model_ll_gold$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_ll_SP500 = npreg(VIX ~ Gold + Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(three_model_ll_SP500)
covariate_range = apply(regression_df[, three_model_ll_SP500$xnames], 2, max) - apply(regression_df[, three_model_ll_SP500$xnames], 2, min)
round(three_model_ll_SP500$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_ll_Nikkei = npreg(VIX ~ Gold + SP500 + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(three_model_ll_Nikkei)
covariate_range = apply(regression_df[, three_model_ll_Nikkei$xnames], 2, max) - apply(regression_df[, three_model_ll_Nikkei$xnames], 2, min)
round(three_model_ll_Nikkei$bw/covariate_range, 2)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
three_model_ll_FTSE = npreg(VIX ~ Gold + SP500 + Nikkei, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(three_model_ll_FTSE)
covariate_range = apply(regression_df[, three_model_ll_FTSE$xnames], 2, max) - apply(regression_df[, three_model_ll_FTSE$xnames], 2, min)
round(three_model_ll_FTSE$bw/covariate_range, 2)

#########################################################################################################################################
#	 Two variables 
regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
two_model_lc_oil = npreg(VIX ~ SP500 + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(two_model_lc_oil)
covariate_range = apply(regression_df[, two_model_lc_oil$xnames], 2, max) - apply(regression_df[, two_model_lc_oil$xnames], 2, min)
round(two_model_lc_oil$bw/covariate_range, 2)

plot_df = data.frame(scale(external_df_original[external_df_original$Date>=min(external_df_testing$Date), variables]))
colnames(plot_df) = variables
plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_testing$Date),c("Date")]
plot_df$type = "Observed"
tmp = data.frame(scale((external_df_testing[, variables[2:length(variables)]])))
tmp$VIX = predict(two_model_lc_oil, newdata = tmp)
tmp$type = 'Fitted'
tmp$Date = external_df_testing$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 3) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.7, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 30), 
    axis.text.x = element_text(size = 30), 
    axis.text = element_text(color = "black", size = 30), 
    axis.title = element_text(color = "black", size = 30)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week")
ggsave("kernel_regression_lc_two_oil_testing.jpeg", plot = p, width = 15, height = 10, dpi = 300)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
two_model_lc_SP500 = npreg(VIX ~ Oil + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(two_model_lc_SP500)
covariate_range = apply(regression_df[, two_model_lc_SP500$xnames], 2, max) - apply(regression_df[, two_model_lc_SP500$xnames], 2, min)
round(two_model_lc_SP500$bw/covariate_range, 2)

plot_df = data.frame(scale(external_df_original[external_df_original$Date>=min(external_df_testing$Date), variables]))
colnames(plot_df) = variables
plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_testing$Date),c("Date")]
plot_df$type = "Observed"
tmp = data.frame(scale((external_df_testing[, variables[2:length(variables)]])))
tmp$VIX = predict(two_model_lc_SP500, newdata = tmp)
tmp$type = 'Fitted'
tmp$Date = external_df_testing$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 3) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.7, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 30), 
    axis.text.x = element_text(size = 30), 
    axis.text = element_text(color = "black", size = 30), 
    axis.title = element_text(color = "black", size = 30)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week")
ggsave("kernel_regression_lc_two_SP500_testing.jpeg", plot = p, width = 15, height = 10, dpi = 300)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
two_model_lc_FTSE = npreg(VIX ~ Oil + SP500, data = regression_df, ckertype = "epanechnikov", regtype = 'lc')
summary(two_model_lc_FTSE)
covariate_range = apply(regression_df[, two_model_lc_FTSE$xnames], 2, max) - apply(regression_df[, two_model_lc_FTSE$xnames], 2, min)
round(two_model_lc_FTSE$bw/covariate_range, 2)

plot_df = data.frame(scale(external_df_original[external_df_original$Date>=min(external_df_testing$Date), variables]))
colnames(plot_df) = variables
plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_testing$Date),c("Date")]
plot_df$type = "Observed"
tmp = data.frame(scale((external_df_testing[, variables[2:length(variables)]])))
tmp$VIX = predict(two_model_lc_FTSE, newdata = tmp)
tmp$type = 'Fitted'
tmp$Date = external_df_testing$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 3) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.7, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 30), 
    axis.text.x = element_text(size = 30), 
    axis.text = element_text(color = "black", size = 30), 
    axis.title = element_text(color = "black", size = 30)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week")
ggsave("kernel_regression_lc_two_FTSE_testing.jpeg", plot = p, width = 15, height = 10, dpi = 300)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
two_model_ll_oil = npreg(VIX ~ Nikkei + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(two_model_ll_oil)
covariate_range = apply(regression_df[, two_model_ll_oil$xnames], 2, max) - apply(regression_df[, two_model_ll_oil$xnames], 2, min)
round(two_model_ll_oil$bw/covariate_range, 2)

plot_df = data.frame(scale(external_df_original[external_df_original$Date>=min(external_df_testing$Date), variables]))
colnames(plot_df) = variables
plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_testing$Date),c("Date")]
plot_df$type = "Observed"
tmp = data.frame(scale((external_df_testing[, variables[2:length(variables)]])))
tmp$VIX = predict(two_model_ll_oil, newdata = tmp)
tmp$type = 'Fitted'
tmp$Date = external_df_testing$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 3) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.7, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 30), 
    axis.text.x = element_text(size = 30), 
    axis.text = element_text(color = "black", size = 30), 
    axis.title = element_text(color = "black", size = 30)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week")
ggsave("kernel_regression_ll_two_SP500_testing.jpeg", plot = p, width = 15, height = 10, dpi = 300)


regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
two_model_ll_Nikkei = npreg(VIX ~ Oil + FTSE, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(two_model_ll_Nikkei)
covariate_range = apply(regression_df[, two_model_ll_Nikkei$xnames], 2, max) - apply(regression_df[, two_model_ll_Nikkei$xnames], 2, min)
round(two_model_ll_Nikkei$bw/covariate_range, 2)

plot_df = data.frame(scale(external_df_original[external_df_original$Date>=min(external_df_testing$Date), variables]))
colnames(plot_df) = variables
plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_testing$Date),c("Date")]
plot_df$type = "Observed"
tmp = data.frame(scale((external_df_testing[, variables[2:length(variables)]])))
tmp$VIX = predict(two_model_ll_Nikkei, newdata = tmp)
tmp$type = 'Fitted'
tmp$Date = external_df_testing$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 3) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.7, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 30), 
    axis.text.x = element_text(size = 30), 
    axis.text = element_text(color = "black", size = 30), 
    axis.title = element_text(color = "black", size = 30)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week")
ggsave("kernel_regression_ll_two_Nikkei_testing.jpeg", plot = p, width = 15, height = 10, dpi = 300)

regression_df = scale(external_df[,2:ncol(external_df)])
regression_df = data.frame(regression_df)
two_model_ll_FTSE = npreg(VIX ~ Oil + Nikkei, data = regression_df, ckertype = "epanechnikov", regtype = 'll')
summary(two_model_ll_FTSE)
covariate_range = apply(regression_df[, two_model_ll_FTSE$xnames], 2, max) - apply(regression_df[, two_model_ll_FTSE$xnames], 2, min)
round(two_model_ll_FTSE$bw/covariate_range, 2)


plot_df = data.frame(scale(external_df_original[external_df_original$Date>=min(external_df_testing$Date), variables]))
colnames(plot_df) = variables
plot_df$Date = external_df_original[external_df_original$Date>=min(external_df_testing$Date),c("Date")]
plot_df$type = "Observed"
tmp = data.frame(scale((external_df_testing[, variables[2:length(variables)]])))
tmp$VIX = predict(two_model_ll_FTSE, newdata = tmp)
tmp$type = 'Fitted'
tmp$Date = external_df_testing$Date
combined_df = rbind(plot_df, tmp)
combined_df$Date = as.Date(combined_df$Date)
p = ggplot(combined_df, aes(x = Date, y = VIX, color = type, group = type)) +
  geom_line(size = 3) + 
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "skyblue", "Fitted" = "pink"))+ 
  theme(
    legend.position = c(0.7, 0.85), 
    legend.background = element_rect(fill = alpha("#F1EDE6", 0.8), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#F1EDE6", color = NA),
    plot.background = element_rect(fill = "#F1EDE6", color = NA),
    axis.text.y = element_text(size = 30), 
    axis.text.x = element_text(size = 30), 
    axis.text = element_text(color = "black", size = 30), 
    axis.title = element_text(color = "black", size = 30)) +
  labs(color = "Line Type") + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week")
ggsave("kernel_regression_ll_two_FTSE_testing.jpeg", plot = p, width = 15, height = 10, dpi = 300)