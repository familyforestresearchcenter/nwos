# 
# 
# Parameters of the Theoretical Population of Family Forest Ownerships
# ========================================================
#   ```{r POP Stats, include=F}
# POP.N <- NROW(POP)
# POP.N.VAR <- 0
# POP.TOTAL <- sum(POP$a)
# POP.TOTAL.VAR <- 0
# POP.MEAN <- mean(POP$a)
# POP.MEAN.VAR <- var(POP$a)
# POP.Q1 <- quantile(POP$a, 0.25)
# POP.Q2 <- quantile(POP$a, 0.50)
# POP.Q3 <- quantile(POP$a, 0.75)
# POP.MIN <- min(POP$a)
# POP.MAX <- max(POP$a)
# PROP.Y1 <- NROW(POP$y1[POP$y1==1]) / NROW(POP$y1)
# PROP.Y1.VAR <- (PROP.Y1 * (1-PROP.Y1))
# PROP.Y2 <- NROW(POP$y2[POP$y2==1]) / NROW(POP$y2)
# PROP.Y2.VAR <- (PROP.Y2 * (1-PROP.Y2))
# ```
# - $N = `r formatC(POP.N, digits=0,big.mark=",",format="f")`$ ownerships
# - $\tau = \Sigma^N{a_i} = `r formatC(POP.TOTAL, digits=0,big.mark=",",format="f")`$ ac
# - $\mu = `r formatC(POP.MEAN, digits=2,big.mark=",",format="f")`$ ac $(SD = `r formatC(sqrt(POP.MEAN.VAR), digits=2,big.mark=",",format="f")`)$
#   - Quartiles
# 
# 0% | 25% | 50% | 75% | 100%
# --- | ---- | ---- | ---- | ----
#   `r formatC(POP.MIN, digits=2,big.mark=",",format="f")` | `r formatC(POP.Q1, digits=2,big.mark=",",format="f")`| `r formatC(POP.Q2, digits=2,big.mark=",",format="f")` | `r formatC(POP.Q3, digits=2,big.mark=",",format="f")`|`r formatC(POP.MAX, digits=2,big.mark=",",format="f")`
# - Y Variables (proportions)
# * $y_1 = `r formatC(PROP.Y1, digits=2,big.mark=",",format="f")`$ $(SD=`r formatC(sqrt(PROP.Y1.VAR), digits=2,big.mark=",",format="f")`)$
#   * $y_2 = `r formatC(PROP.Y2, digits=2,big.mark=",",format="f")`$ $(SD=`r formatC(sqrt(PROP.Y2.VAR), digits=2,big.mark=",",format="f")`)$
# 
# 
# 
#   Calculate Weighted Parametric Estimates
# ========================================================
#   ```{r Weighted SAMPLE Stats Parametric}
# N_HAT_W <- sum(SAMPLE$w_f)
# sumFunc <- function(x,i){sum(x[i])*(n/n_s)}
# b <- boot(SAMPLE$w_b, sumFunc, 1000)
# N_HAT_W_VAR <- var(b$t)
# MEAN_HAT_W <- wtd.mean(SAMPLE$a, SAMPLE$w_f)
# MEAN_HAT_W_VAR <- wtd.var(SAMPLE$a, SAMPLE$w_f)
# TOTAL_HAT_W <- N_HAT_W * MEAN_HAT_W
# # totFunc <- function(data, i) {d<-data[i,];return(wtd.mean(d$a,d$w_f)*sum(d$w_f[i])*(n/(n_s)*0.75))}
# # b2 <- boot(data=SAMPLE[,c("a","w_f")], statistic=totFunc, R=1000)
# b2 <- boot.var(x=SAMPLE, func='total', R=1000)
# TOTAL_HAT_W_VAR <- var(b2)
# ```
# 
# 
# 
# Calculate Weighted Y Estimates
# ========================================================
#   ```{r Weighted SAMPLE Stats Y}
# # PROP_Y1_HAT_W <- NROW(SAMPLE$y1[SAMPLE$y1==1]) / NROW(SAMPLE$y1)
# PROP_Y1_HAT_W <- wtd.mean(SAMPLE$y1, SAMPLE$w_f)
# PROP_Y1_HAT_W_VAR <- (PROP_Y1_HAT_W*(1-PROP_Y1_HAT_W))/(NROW(SAMPLE)-1)
# PROP_Y1_HAT_TW <- wtd.mean(SAMPLE$y1, SAMPLE$w_t)
# PROP_Y1_HAT_TW_VAR <- (PROP_Y1_HAT_TW*(1-PROP_Y1_HAT_TW))/(NROW(SAMPLE)-1)
# # PROP_Y2_HAT_W <- NROW(SAMPLE$y2[SAMPLE$y2==1]) / NROW(SAMPLE$y2)
# PROP_Y2_HAT_W <- wtd.mean(SAMPLE$y2, SAMPLE$w_f)
# PROP_Y2_HAT_W_VAR <- (PROP_Y2_HAT_W*(1-PROP_Y2_HAT_W))/(NROW(SAMPLE)-1)
# PROP_Y2_HAT_TW <- wtd.mean(SAMPLE$y2, SAMPLE$w_t)
# PROP_Y2_HAT_TW_VAR <- (PROP_Y2_HAT_TW*(1-PROP_Y2_HAT_TW))/(NROW(SAMPLE)-1)
# ```
