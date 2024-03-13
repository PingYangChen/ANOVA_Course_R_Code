


n_sim <- 1000
indv_res <- vector("list", length = n_sim)
for (i in 1:n_sim) {
  set.seed(i) #
  #  
  y <- mu_t + rnorm(n*a, 0, sig)
  fit <- aov(y ~ x)
  fit_sum <- summary(fit)
  pval <- fit_sum[[1]][1, 5]
  indv_res[[i]] <- list(
    "y" = y,
    "fit" = fit,
    "signif" = pval < alpha
  )
}
indv_signif <- sapply(1:n_sim, function(i) indv_res[[i]]$signif)
sim_t1e <- sum(indv_signif)/n_sim



sim_aov <- function(n_sim = 1000, alpha = 0.05, sim_type = 1, 
                    n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
                    dist_func = rnorm, ...) {
  a <- length(tau_t)
  x <- as.factor(rep(1:a, each = n))
  indv_res <- vector("list", length = n_sim)
  for (i in 1:n_sim) {
    set.seed(i) #
    #
    if (sim_type == 1) {
      y <- mu_t + dist_func(n*a, ...)  
    } else if (sim_type == 2) {
      y <- mu_t + rep(tau_t, each = n) + dist_func(n*a, ...)
    }
    #
    fit <- aov(y ~ x)
    fit_sum <- summary(fit)
    pval <- fit_sum[[1]][1, 5]
    indv_res[[i]] <- list(
      "y" = y,
      "fit" = fit,
      "signif" = pval < alpha
    )
  }
  return(indv_res)
}


res_norm_1 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 1, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = rnorm, mean = 0, sd = 2
)

norm_1_signif <- sapply(1:length(res_norm_1), function(i) res_norm_1[[i]]$signif)
norm_1_err <- sum(norm_1_signif)/length(res_norm_1)


res_norm_2 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 2, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = rnorm, mean = 0, sd = 2
)
norm_2_signif <- sapply(1:length(res_norm_2), function(i) res_norm_2[[i]]$signif)
norm_2_err <- sum(1 - norm_2_signif)/length(res_norm_2)



res_exp_1 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 1, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = rexp, rate = 0.5
)

exp_1_signif <- sapply(1:length(res_exp_1), function(i) res_exp_1[[i]]$signif)
exp_1_err <- sum(exp_1_signif)/length(res_exp_1)


res_exp_2 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 2, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = rexp, rate = 0.5
)

exp_2_signif <- sapply(1:length(res_exp_2), function(i) res_exp_2[[i]]$signif)
exp_2_err <- sum(1 - exp_2_signif)/length(res_exp_2)



# v/(v-2) = 4
# v = 4v - 8
# v = 8/3
res_t_1 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 1, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = rt, df = 8/3
)

t_1_signif <- sapply(1:length(res_t_1), function(i) res_t_1[[i]]$signif)
t_1_err <- sum(t_1_signif)/length(res_t_1)

# v/(v-2) = 4
# v = 4v - 8
# v = 8/3
res_t_2 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 2, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = rt, df = 8/3
)

t_2_signif <- sapply(1:length(res_t_2), function(i) res_t_2[[i]]$signif)
t_2_err <- sum(1 - t_2_signif)/length(res_t_2)


# e \in unif(-b, b)
# var(e) = (2b)^2 / 12 = b^2 / 3 === 4
# b = sqrt(12)
res_unif_1 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 1, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = runif, min = -sqrt(12), max = sqrt(12)
)

unif_1_signif <- sapply(1:length(res_unif_1), function(i) res_unif_1[[i]]$signif)
unif_1_err <- sum(unif_1_signif)/length(res_unif_1)

res_unif_2 <- sim_aov(
  n_sim = 1000, alpha = 0.05, sim_type = 2, 
  n = 5, mu_t = 0, tau_t = c(-3, -1, 1, 3), 
  dist_func = runif, min = -sqrt(12), max = sqrt(12)
)

unif_2_signif <- sapply(1:length(res_unif_2), function(i) res_unif_2[[i]]$signif)
unif_2_err <- sum(1 - unif_2_signif)/length(res_unif_2)


