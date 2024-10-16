library(splines)
library(mgcv)
library(plotly)
external_df = read.csv('external_df.csv')
external_df = external_df[-which(external_df$Oil<0),]
external_df_unchanged = external_df
external_df = data.frame(scale(external_df[,2:ncol(external_df)]))
variables = c("VIX", "Oil", "Gold",  "SP500", "Nikkei", "SSE", "FTSE")
external_df_original = external_df
set.seed(999)
training_index = sample(1:nrow(external_df), 1600)
external_df = external_df_original[training_index, ]
external_df_test = external_df_original[-training_index, ]
########################################################################
#Regression Spline 
########################################################################
########################################################################
#B-Splines
########################################################################
num_knots = 3:50
CV_scores_RS_BS = c()
for (i in 1:length(num_knots)){
  RS_BS = lm(external_df$VIX ~ bs(external_df$Oil, df = num_knots[i]))
  B = model.matrix(RS_BS)
  omega = B%*%solve(t(B)%*%B)%*%t(B)
  diag_omega = diag(omega)
  y_hat = fitted(RS_BS)
  w = (external_df$Oil > quantile(external_df$Oil, 0.05)) & (external_df$Oil < quantile(external_df$Oil, 0.95))
  cv_score = mean(((external_df$VIX - y_hat) / (1 - diag_omega))^2 * w)
  CV_scores_RS_BS[i] = cv_score
}
print(which.min(CV_scores_RS_BS) + 2)

optimal_knots = which.min(CV_scores_RS_BS) + 2
spline_basis <- bs(external_df$Oil, df = optimal_knots)
internal_knots <- attr(spline_basis, "knots")
boundary_knots <- attr(spline_basis, "Boundary.knots")
spline_basis_df <- as.data.frame(spline_basis)
colnames(spline_basis_df) <- paste0("spline_", seq_len(ncol(spline_basis_df)))
RS_BS <- lm(external_df$VIX ~ ., data = spline_basis_df)

new_data_spline <- as.data.frame(bs(external_df_test$Oil, knots = internal_knots, Boundary.knots = boundary_knots))
colnames(new_data_spline) <- colnames(spline_basis_df)
predicted_VIX <- predict(RS_BS, newdata = new_data_spline)
print(optimal_knots)
print(mean((RS_BS$residuals)^2))
print(mean((external_df_test$VIX-predicted_VIX)^2))

regression_df = external_df[,c('VIX', 'Oil')]
regression_df$Type <- "Observed"
fitted_df <- data.frame(Oil = regression_df$Oil, VIX = fitted(RS_BS), Type = "Fitted")
combined_df <- rbind(regression_df, fitted_df)

p = ggplot(data = combined_df, aes(x = Oil, y = VIX, color = Type)) + 
  geom_point(data = subset(combined_df, Type == "Observed"), alpha = 1) + 
  geom_line(data = subset(combined_df, Type == "Fitted"), size = 2) + 
  scale_color_manual(values = c("Observed" = "pink", "Fitted" = "black")) +
  labs(title = "", x = "Oil", y = "VIX", color = "Legend") +
  theme_minimal() + 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#8497B5"), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#8497B5", color = NA),
    plot.background = element_rect(fill = "#8497B5", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 30),    
    axis.title.y = element_text(size = 30),
  )
ggsave("bspline_regression_spline.jpeg", p, bg = "#8497B5", width = 16, height = 8)
########################################################################
#Cubic Splines
########################################################################
num_knots = 3:50
CV_scores_RS_NS = c()
for (i in 1:length(num_knots)){
  RS_NS = lm(external_df$VIX ~ ns(external_df$Oil, df = num_knots[i]))
  B = model.matrix(RS_NS)
  omega = B%*%solve(t(B)%*%B)%*%t(B)
  diag_omega = diag(omega)
  y_hat = fitted(RS_NS)
  w = (external_df$Oil > quantile(external_df$Oil, 0.05)) & (external_df$Oil < quantile(external_df$Oil, 0.95))
  cv_score = mean(((external_df$VIX - y_hat) / (1 - diag_omega))^2 * w)
  CV_scores_RS_NS[i] = cv_score
}
print(which.min(CV_scores_RS_NS) + 2)

optimal_knots = which.min(CV_scores_RS_NS) + 2
spline_basis <- bs(external_df$Oil, df = optimal_knots)
internal_knots <- attr(spline_basis, "knots")
boundary_knots <- attr(spline_basis, "Boundary.knots")
spline_basis_df <- as.data.frame(spline_basis)
colnames(spline_basis_df) <- paste0("spline_", seq_len(ncol(spline_basis_df)))
RS_NS <- lm(external_df$VIX ~ ., data = spline_basis_df)

new_data_spline <- as.data.frame(bs(external_df_test$Oil, knots = internal_knots, Boundary.knots = boundary_knots))
colnames(new_data_spline) <- colnames(spline_basis_df)
predicted_VIX <- predict(RS_NS, newdata = new_data_spline)
print(optimal_knots)
print(mean((RS_NS$residuals)^2))
print(mean((external_df_test$VIX-predicted_VIX)^2))

regression_df = external_df[,c('VIX', 'Oil')]
regression_df$Type <- "Observed"
fitted_df <- data.frame(Oil = regression_df$Oil, VIX = fitted(RS_NS), Type = "Fitted")
combined_df <- rbind(regression_df, fitted_df)

p = ggplot(data = combined_df, aes(x = Oil, y = VIX, color = Type)) + 
  geom_point(data = subset(combined_df, Type == "Observed"), alpha = 1) + 
  geom_line(data = subset(combined_df, Type == "Fitted"), size = 2) + 
  scale_color_manual(values = c("Observed" = "#8FCACA", "Fitted" = "black")) +
  labs(title = "", x = "Oil", y = "VIX", color = "Legend") +
  theme_minimal() + 
  theme(
    legend.position = c(0.9, 0.85), 
    legend.background = element_rect(fill = alpha("#8497B5"), color = NA),  
    legend.text = element_text(size = 30),       
    legend.title = element_text(size = 30),     
    legend.key.size = unit(4, "lines"),
    panel.background = element_rect(fill = "#8497B5", color = NA),
    plot.background = element_rect(fill = "#8497B5", color = NA),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 30),    
    axis.title.y = element_text(size = 30),
  )
ggsave("cubic_regression_spline.jpeg", p, bg = "#8497B5", width = 16, height = 8)



########################################################################
#Penalized Regression Spline 
########################################################################
########################################################################
# #Cubic-Splines
plots = list()
sp_list = c(0, 10, 100, 1000)
for (i in 1:4){
  PS_BS <- gam(external_df$VIX ~ s(external_df$Oil, bs = "cr", sp = sp_list[i]))
  regression_df = external_df[,c('VIX', 'Oil')]
  regression_df$Type <- "Observed"
  fitted_df <- data.frame(Oil = regression_df$Oil, VIX = fitted(PS_BS), Type = "Fitted")
  combined_df <- rbind(regression_df, fitted_df)
  
  plots[[i]] <- ggplot(data = combined_df, aes(x = Oil, y = VIX, color = Type)) + 
    geom_point(data = subset(combined_df, Type == "Observed"), alpha = 0.8) + 
    geom_line(data = subset(combined_df, Type == "Fitted"), size = 1) + 
    scale_color_manual(values = c("Observed" = "pink", "Fitted" = "black")) +
    labs(
      title = paste0("Penalized Cubic Spline Regression with lambda = ", as.character(sp_list[i])),
      x = "Oil",
      y = "VIX",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 7),  
      legend.position = c(0.8, 0.8),
      axis.title.x = element_text(size = 5), 
      axis.title.y = element_text(size = 5), 
      axis.text.x = element_text(size = 5), 
      axis.text.y = element_text(size = 5), 
      legend.title = element_text(size = 6),  
      legend.text = element_text(size = 6)   
    )
}

grid_plot <- grid.arrange(grobs = plots, ncol = 4, nrow = 1)
ggsave("penalized_cubic_regression_spline_with_different_lambda.jpeg", plot = grid_plot, width = 12, height = 2, dpi = 300)

PS_BS <- gam(external_df$VIX ~ s(external_df$Oil, bs = "bs"))
regression_df = external_df[,c('VIX', 'Oil')]
regression_df$Type <- "Observed"
fitted_df <- data.frame(Oil = regression_df$Oil, VIX = fitted(PS_BS), Type = "Fitted")
combined_df <- rbind(regression_df, fitted_df)

p = ggplot(data = combined_df, aes(x = Oil, y = VIX, color = Type)) + 
  geom_point(data = subset(combined_df, Type == "Observed"), alpha = 0.8) + 
  geom_line(data = subset(combined_df, Type == "Fitted"), size = 1) + 
  scale_color_manual(values = c("Observed" = "pink", "Fitted" = "black")) +
  labs(title = paste0("Penalized Cubic Spline Regression with Optimal Lambda = ", as.character(PS_BS$sp)), x = "Oil", y = "VIX", color = "Legend") +
theme_minimal()
ggsave("penalized_cubic_regression_spline_with_optimal_lambda.jpeg", plot = grid_plot, width = 12, height = 10, dpi = 300)


# model_gam_bs <- gam(y ~ s(x, bs = "bs", k = 10))
# #Cubic - Splines
# model_gam <- gam(y ~ s(x, bs = "cr"))


########################################################################
#Tesor product  Spline 
########################################################################
########################################################################

Tensor_spline <- gam(VIX ~ te(Oil, Gold, SP500, Nikkei, SSE, FTSE, k = rep(3,6), sp = rep(0,6)), data = external_df)
predict_test = predict(Tensor_spline, newdata = external_df_test[,2:7])
mean((Tensor_spline$residuals)^2)
mean((external_df_test$VIX-predict_test)^2)

Tensor_spline <- gam(VIX ~ te(Oil, SP500, Nikkei, SSE, FTSE, k = rep(3,5), sp = rep(0,5)), data = external_df)
predict_test = predict(Tensor_spline, newdata = external_df_test[,2:7])
mean((Tensor_spline$residuals)^2)
mean((external_df_test$VIX-predict_test)^2)

Tensor_spline <- gam(VIX ~ te(Oil, SP500, Nikkei, FTSE, k = rep(3,4), sp = rep(0,4)), data = external_df)
predict_test = predict(Tensor_spline, newdata = external_df_test[,2:7])
mean((Tensor_spline$residuals)^2)
mean((external_df_test$VIX-predict_test)^2)

png(filename = "tensor_spline_plot_four_model.png", width = 800, height = 600)
par(bg = "#8497B5")
plot(Tensor_spline, scheme = 1, pages = 1, too.far = 0.1)
dev.off()

Tensor_spline <- gam(VIX ~ te(SP500, Nikkei, FTSE, k = rep(3,3), sp = rep(0,3)), data = external_df)
predict_test = predict(Tensor_spline, newdata = external_df_test[,2:7])
mean((Tensor_spline$residuals)^2)
mean((external_df_test$VIX-predict_test)^2)

png(filename = "tensor_spline_plot_three_model.png", width = 800, height = 600)
par(bg = "#8497B5")
plot(Tensor_spline, scheme = 1, pages = 1, too.far = 0.1)
dev.off()

Tensor_spline <- gam(VIX ~ te(Nikkei, SP500, k = rep(2,3), sp = rep(0,2)), data = external_df)
predict_test = predict(Tensor_spline, newdata = external_df_test[,2:7])
mean((Tensor_spline$residuals)^2)
mean((external_df_test$VIX-predict_test)^2)

png(filename = "tensor_spline_plot_two_model_Nikkei.png", width = 800, height = 600)
par(bg = "#8497B5")
plot(Tensor_spline, scheme = 1, pages = 1, too.far = 0.1)
dev.off()

Tensor_spline <- gam(VIX ~ te(Oil, SP500, k = c(3, 3), sp = c(0, 0)), data = external_df)
predict_test = predict(Tensor_spline, newdata = external_df_test[,c('Oil', 'SP500')])
mean((Tensor_spline$residuals)^2)
mean((external_df_test$VIX-predict_test)^2)
par(bg = "#8497B5") 
plot(Tensor_spline, scheme = 1, pages = 1, too.far = 0.1)
png(filename = "tensor_spline_plot_two_model_oil.png", width = 800, height = 600)
dev.off()

L1_list = 1:30
L2_list = 1:30
gcv_score = matrix(nrow = 30, ncol = 30)

for (i in L1_list){
  for (j in L2_list){
    Tensor_spline <- gam(VIX ~ te(Oil, SP500, k = c(L1_list[i], L2_list[j]), sp = c(0, 0)), data = external_df)
    gcv_score[i,j] = Tensor_spline$gcv.ubre
  }
}
print(gcv_score)
min_value <- min(gcv_score)
min_position <- which(gcv_score == min_value, arr.ind = TRUE)






# regression_df = external_df[,c('VIX', 'Oil')]
# regression_df$Type <- "Observed"
# fitted_df <- data.frame(Oil = regression_df$Oil, VIX = fitted(Tensor_spline), Type = "Fitted")
# combined_df <- rbind(regression_df, fitted_df)

# p = ggplot(data = combined_df, aes(x = Oil, y = VIX, color = Type)) + 
#   geom_point(data = subset(combined_df, Type == "Observed"), alpha = 0.8) + 
#   geom_line(data = subset(combined_df, Type == "Fitted"), size = 1) + 
#   scale_color_manual(values = c("Observed" = "#8FCACA", "Fitted" = "black")) +
#   labs(title = "Cubic Spline Regression Fit", x = "Oil", y = "VIX", color = "Legend") +
#   theme_minimal()





Tensor_spline <- gam(VIX ~ te(Oil, SP500, k = c(3, 3), sp = c(0, 0)), data = external_df)
Tensor_spline$smooth[[1]]$margin[[1]]$bs.dim
Tensor_spline$smooth[[1]]$margin[[2]]$bs.dim
print(Tensor_spline$gcv.ubre)

Tensor_spline <- gam(VIX ~ te(Oil, SP500, k = c(3, 4), sp = c(0, 0)), data = external_df)
Tensor_spline$smooth[[1]]$margin[[1]]$bs.dim
Tensor_spline$smooth[[1]]$margin[[2]]$bs.dim
print(Tensor_spline$gcv.ubre)

Tensor_spline <- gam(VIX ~ te(Oil, SP500, k = c(3, 5), sp = c(0, 0)), data = external_df)
Tensor_spline$smooth[[1]]$margin[[1]]$bs.dim
Tensor_spline$smooth[[1]]$margin[[2]]$bs.dim
print(Tensor_spline$gcv.ubre)

Tensor_spline <- gam(VIX ~ te(Oil, SP500, k = c(3, 10), sp = c(0, 0)), data = external_df)
Tensor_spline$smooth[[1]]$margin[[1]]$bs.dim
Tensor_spline$smooth[[1]]$margin[[2]]$bs.dim
print(Tensor_spline$gcv.ubre)

Tensor_spline <- gam(VIX ~ te(Oil, SP500, sp = c(0, 0)), data = external_df)
Tensor_spline$smooth[[1]]$margin[[1]]$bs.dim
Tensor_spline$smooth[[1]]$margin[[2]]$bs.dim
print(Tensor_spline$gcv.ubre)





# oil_seq <- seq(min(external_df$Oil), max(external_df$Oil), length.out = 50)
# sp500_seq <- seq(min(external_df$SP500), max(external_df$SP500), length.out = 50)
# grid_df <- expand.grid(Oil = oil_seq, SP500 = sp500_seq)
# grid_df$VIX_fit <- predict(Tensor_spline, newdata = grid_df)
# 
# plot_ly() %>%
#   add_markers(data = external_df, x = ~Oil, y = ~SP500, z = ~VIX, 
#               marker = list(color = 'blue', opacity = 0.5), name = "Observed") %>%
#   add_surface(x = ~oil_seq, y = ~sp500_seq, z = matrix(grid_df$VIX_fit, 50, 50), 
#               colorscale = 'Viridis', opacity = 0.7, name = "Fitted Surface") %>%
#   layout(scene = list(
#     xaxis = list(title = "Oil"),
#     yaxis = list(title = "SP500"),
#     zaxis = list(title = "VIX"),
#     title = "3D Tensor Product Spline Regression"
#   ))
