library(dplyr)
library(reticulate)
library(ggplot2)
library(reshape2)
library(deSolve)
library(viridisLite)
library(viridis)
library(pracma)
library(igraph)
library(ggraph)
library(grid)
library(gridExtra)

###############################################################################
# empirical data from Li et al


# parameters taken for the Varying size ratio model with temperature for aquatic
# species
#                 \beta_x,0 \beta_x,1    \beta_x,temp

beta_base <- matrix(c(3.01503, 0.4600, -0.0738,   # mu_i
                      1.6645,  0.2172, -0.0912,   # sigma_i
                      -1.1012, -0.1533, 0.0309),  # theta_i
                    nrow=3, byrow=TRUE)

beta <- beta_base
# change from f_i = m_i - mu_i
beta[1,] <- -beta[1,]
beta[1, 2] <- beta[1, 2] + 1
# change to natrual log instead of log10
beta[, 2] <- beta[, 2] / log(10)


#issue is with uiterwaal, small species don't eav fast enough'
# what dimensions do we have in mind?

generate_species <- function(n_spec, e = 0.45, K = 1e7, c_intra = 0.01, random = TRUE, B0 = 1e-7) {
  
  species_id <- list(
    m_i = 10^(-rnorm(n_spec, mean = 0, sd = 2)),
    e = e,
    K = K,
    c_intra = c_intra
  )
  
  species_id$m_i <- sort(species_id$m_i)
  
  if (!random) {
    species_id$m_i <- 10^seq(-6, 4, length.out = n_spec)
  }
  
  if (!is.null(B0)) {
    species_id$m_i[1] <- B0
  }
  
  species_id$random_state <- sample.int((2^30)-1, 1)
  
  return(species_id)
}

traits <- c("f_i", "sig_i", "theta_i")

change_temperature <- function(species_id, T = 20, beta) {
  delta_T <- T - 20 # Li et al used temperatures relative to 20°C
  
  for (i in seq_along(traits)) {
    trait <- traits[i]
    species_id[[trait]] <- beta[i, 1] + beta[i, 2] * log(species_id$m_i) + beta[i, 3] * delta_T
  }
  
  # transformation of the traits
  # adjust that body mass is given in natural log
  species_id$f_i <- exp(log(10) * species_id$f_i)
  # adjust that body mass is given in natural log
  species_id$sig_i <- log(10) * sqrt(exp(species_id$sig_i))
  species_id$theta_i <- exp(species_id$theta_i)
  species_id$theta_i <- species_id$theta_i / (1 + species_id$theta_i)
  species_id$f_i[1] <- 1e-50
  
  return(species_id)
}


# For plotting and testing
n_spec <- 101
species_id_base <- generate_species(n_spec)
species_id_base$m_i <- 10^seq(-12, 8, length.out = n_spec)
species_id_base <- change_temperature(species_id_base, beta = beta)

compute_predation_prob <- function(species_id) {
  log_mi <- log(species_id$m_i)
  log_fi <- log(species_id$f_i)
  log_mi_matrix <- matrix(log_mi, nrow = length(log_mi), ncol = length(log_mi), byrow = TRUE)
  
  # Preference matrix
  s_ji <- (species_id$theta_i 
           * exp(-(log_mi_matrix - log_fi)^2/(2 * species_id$sig_i^2)))
  
  return(s_ji)
}

compute_links <- function(species_id) {
  set.seed(species_id$random_state)
  s_ji <- compute_predation_prob(species_id)
  threshold <- matrix(runif(length(s_ji), 0.05, 1), nrow = nrow(s_ji), ncol = ncol(s_ji))
  return(1.0 * (s_ji > threshold))
}

if (interactive()) {
  s_ji <- compute_predation_prob(species_id_base)
  extent <- quantile(log10(species_id_base$m_i), probs = c(0, 1))
  df1 <- expand.grid(
    Predator = seq(extent[1], extent[2], length.out = nrow(s_ji)),
    Prey = seq(extent[1], extent[2], length.out = ncol(s_ji))
  )
  df1$PredationProb <- as.vector(s_ji)
}

## Predation probability
contour_colors <- c("darkred", "red", "orange", "white", "lightgrey", "#619CFF", "darkblue")

bi1 <- ggplot(df1, aes(x = Predator, y = Prey, fill = PredationProb)) + 
  geom_tile() + 
  scale_fill_viridis_c(
    breaks = seq(0.0, 0.6, by = 0.1),
    labels = paste(seq(0.0, 0.6, by = 0.1)),
    guide = guide_colorbar(barwidth = 0.9, barheight = 17)
  ) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  geom_contour(aes(z = PredationProb, color = after_stat(level)), 
               breaks = c(0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60),  
               linewidth = 1) + 
  scale_color_gradientn(
    colors = contour_colors,
    breaks = c(0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60),  
    labels = paste(c(0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60)),  
    guide = guide_colorbar(barwidth = 0.9, barheight = 17)) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) + 
  labs(
    title = "Predation probability",
    x = "Predator bodysize [log]",
    y = "Prey bodysize [log]"
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = "black"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 13, color = "black"),
    axis.title.y = element_text(size = 13, color = "black"),
    legend.direction = "vertical", 
    legend.box = "horizontal"  
  )

bi1


########################################################################################
# Empirical data from Uiterwaal et al.
# 3D predation space

# Empirical data from Uiterwaal et al.
forage_attack <- list(
  intercept = -15.92,    # intercept of regression
  temp_2 = -0.01,        # quadratic effect of temperature
  temp = 0.32,           # linear effect of temperature
  log_pred = 0.54,       # effect of predator mass
  log_prey = 0.05,       # effect of prey mass
  T_ref = 26.7           # reference temperature for fit
)

compute_attack_rate <- function(species_id, T = 20) {
  # Basic attack rate
  log_mi <- log(species_id$m_i)
  log_mi_matrix <- matrix(log_mi, nrow = length(log_mi), ncol = length(log_mi), byrow = TRUE)
  
  interm <- list(
    temp_2 = (T - forage_attack$T_ref)^2,
    temp = T - forage_attack$T_ref,
    log_pred = log_mi,
    log_prey = log_mi_matrix)
  
  # Compute attack rate in log space
  attack_rate <- forage_attack$intercept
  for (key in names(interm)) {
    attack_rate <- attack_rate + interm[[key]] * forage_attack[[key]]
  }
  
  # Convert from log space to normal space
  attack_rate <- exp(attack_rate)
  
  # Convert from attack rate/individual to attack rate per kg
  attack_rate <- sweep(attack_rate, 2, species_id$m_i, "/")
  
  return(attack_rate)
}

# Plot basic result of Uiterwaal
if (interactive()) {
  attack_rate <- compute_attack_rate(species_id_base)
  
  # Define 'extent' based on the log10 body mass range
  extent <- quantile(log10(species_id_base$m_i), probs = c(0, 1))
  attack_rate_data <- expand.grid(
    Predator = seq(extent[1], extent[2], length.out = nrow(attack_rate)),
    Prey = seq(extent[1], extent[2], length.out = ncol(attack_rate))
  )
  attack_rate_data$Attack_rate <- as.vector(log(attack_rate))
}

# Plot basic result of Uiterwaal
bi2 <- ggplot(attack_rate_data, aes(x = Predator, y = Prey, fill = Attack_rate)) +
  geom_tile() +
  scale_fill_viridis_c(
    breaks = seq(-50, 10, by = 10),   
    labels = paste(seq(-50, 10, by = 10)),
    guide = guide_colorbar(
      barwidth = 0.9,  
      barheight = 17
    )
  ) +
  labs(
    title = "Attack rate",
    x = "Predator bodysize [log]",
    y = "Prey bodysize [log]"
  ) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, color = "black"),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 11, color = "black"), 
        axis.text.y = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 13, color = "black"),
        axis.title.y = element_text(size = 13, color = "black")) +
  scale_y_continuous(breaks = seq(-10, 7.5, 2.5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) 
bi2

###############################################################################
# combine Li and Uiterwaal to create actual food web model

compute_LV_param <- function(species_id, T = 20) {
  
  # get predation matrix
  links_ji <- compute_links(species_id)
  
  # regression obtained from Li et al.
  attack_rate_ji <- compute_attack_rate(species_id, T)
  
  # compute predation strength
  pred_ji <- links_ji*attack_rate_ji
  
  # Lotka_Volterra interaction rate
  a_ij <- (-t(species_id$e * pred_ji)+(pred_ji)+diag(length(species_id$sig_i))*species_id$c_intra)

  
  a_ij[1,1] <- 1e0
  # cap maximal species interactions
  a_ij[abs(a_ij) > 2] <- 2 * sign(a_ij[abs(a_ij) > 2])
  
  mu <- -0.314 * species_id$m_i^(0.22)
  mu[1] <- species_id$K * a_ij[1,1]
  
  return(list(mu = mu, A = t(a_ij)))
}

compute_trophic_level <- function(species_id, present = NULL) {
  
  s_ji <- t(compute_links(species_id))
  
  if (!is.null(present)) {
    s_ji <- s_ji[present, present, drop = FALSE]
  }
  
  s_ji <- s_ji / rowSums(s_ji, na.rm = TRUE)
  s_ji[is.na(s_ji)] <- 0  
  trophic_level <- tryCatch({
    solve(diag(nrow(s_ji)) - s_ji, rep(1, nrow(s_ji)))
  }, error = function(e) {
    rep(NaN, nrow(s_ji))  
  })
  
  return(trophic_level - 1) 
}

plot_foodweb <- function(species_id, surv = NULL) {
  s_ij <- compute_links(species_id)
  trophic_level <- compute_trophic_level(species_id, surv)
  # spread species according to the trophic level
  ind <- sort(trophic_level)  
  n_stages <- 4
  len_ind <- length(ind)
  base_per_stage <- (len_ind - 1) %/% n_stages
  remainder <- (len_ind - 1) %% n_stages
  
  n_per_stage <- rep(base_per_stage, n_stages)
  n_per_stage[1:remainder] <- n_per_stage[1:remainder] + 1
  
  x_loc <- c(0.5, unlist(lapply(n_per_stage, function(n) seq(0, 1, length.out = n + 2)[2:(n + 1)])))
  x_loc <- x_loc[ind]
  
  plot_data <- data.frame(x = x_loc, y = trophic_level)
  
  edges <- which(s_ij != 0, arr.ind = TRUE)
  edge_data <- data.frame(
    x = x_loc[edges[, 1]],
    y = trophic_level[edges[, 1]],
    xend = x_loc[edges[, 2]],
    yend = trophic_level[edges[, 2]]
  )
  ggplot() +
    geom_segment(data = edge_data, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), alpha = 0.1) +
    geom_point(data = plot_data, aes(x = x, y = y)) +
    theme_minimal() +
    labs(x = "", y = "Trophic Level")
}

#################################################################################################

# Plot results from both combined
imu_A <- compute_LV_param(species_id_base)
A <- imu_A$A
mu <- imu_A$mu



## Magnitude of interaction 
bi4 <- ggplot(A_df, aes(x = Predator, y = Prey, fill = InteractionMagnitude)) +
  geom_point() +
  geom_raster() +
  scale_fill_viridis_c(guide = guide_colorbar( barwidth = 0.9, barheight = 17),
                       breaks = seq(-20, 0, by = 5),   
                       labels = paste(seq(-20, 0, by = 5))) +
  labs(title = "Magnitude of interaction", x = "Predator bodysize [log]", y = "Prey bodysize [log]") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, color = "black"),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 11, color = "black"), 
        axis.text.y = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 13, color = "black"),
        axis.title.y = element_text(size = 13, color = "black")) +
  scale_y_continuous(breaks = seq(-10, 7.5, 2.5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

bi4

###############################################################################
# plot species traits and feeding preferences

species_id <- generate_species(100, random = FALSE, B0 = 1e-8)
species_id <- change_temperature(species_id, beta = beta)

imu_A_t <- compute_LV_param(species_id)
mu_t <- imu_A_t$mu
A_t <- imu_A_t$A

imu_A_d <- as.data.frame(imu_A_t)

imu_A_d$mu[-1]

# Extract variables 
sigi <- species_id$sig_i[-1]
mi <- log10(species_id$m_i)[-1]
fi <- log10(species_id$f_i)[-1]
thetai <- species_id$theta_i[-1]

trait_data <- data.frame(
  sigi = sigi,
  mi = mi,
  fi = fi,
  thetai = thetai)


# Plot 1: Feeding preference vs. predator body size
p1 <- ggplot() +
  geom_line(aes(x = mi, y = fi, color = "f_i")) +
  geom_line(aes(x = mi, y = mi, color = "m_i")) +
  labs(x = "Predator bodysize [log]", y = "Feeding preference") +
  scale_color_manual(values = c("f_i" = "#89CFF0", "m_i" = "orange"),
                     labels = c(expression(f[i]), expression(m[i]))) +
  theme_bw() +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = c(0.08, 0.9),
        legend.background = element_rect(fill = "transparent", color = "lightgrey"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_y_continuous(breaks = seq(-6, 4, 2)) 
p1

# Plot 2: Niche width and predation probability vs. predator body size

p2 <- ggplot(trait_data, aes(x = mi)) +
  geom_line(aes(y = sigi, color = "sigi")) +
  geom_line(aes(y = thetai * 17.6, color = "thetai")) +  
  labs(x = "Predator bodysize [log]",
       y = expression("Niche width," * sigma[i])) +
  scale_color_manual(values = c("sigi" = "#89CFF0", "thetai" = "red"),
                     labels = c(expression(sigma[i]), expression(theta[i]))) +
  scale_y_continuous(breaks = seq(2, 8, 1),
                     sec.axis = sec_axis(
                       trans = ~ . / 17.6,  
                       breaks = seq(0.15, 0.55, 0.05),
                       name = expression("Total predation probability," * theta[i])),
                     limits = c(2, 8)) +
  theme_bw() +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = "transparent", color = "lightgrey"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank()) 
p2

s_ji <- compute_predation_prob(species_id)
s_ji.t <- t(s_ji)

# Plot 3: Probability of eating B0 and mean number of links
p3 <- ggplot() +
  geom_line(aes(x = mi, y = s_ji.t[1, -1], color = "Prob. of eating B0")) +
  geom_hline(aes(yintercept = mean(s_ji.t[1, -1]), color = "Mean eating B0"), linetype = "dashed") +
  geom_line(aes(x = mi, y = colMeans(s_ji.t[, -1]), color = "Mean number of links")) +
  geom_hline(aes(yintercept = mean(colMeans(s_ji.t[, -1])), color = "Mean of (mean number of links)"), linetype = "dashed") +
  labs(x = "Predator bodysize [log]", y = "Probability of eating B0") +
  scale_color_manual(values = c("Prob. of eating B0" = "red", "Mean eating B0" = "red",
                                "Mean number of links" = "blue", "Mean of (mean number of links)" = "blue")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = c(0.8, 0.5),
        legend.background = element_rect(fill = "transparent", color = "lightgrey"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 0.14, 0.02))
p3

# Compute hypothetical A if all species were linked
s_ji_hypothetical <- matrix(1, nrow = nrow(s_ji), ncol = ncol(s_ji))
attack_rate_ji <- compute_attack_rate(species_id)
pred_ji <- s_ji_hypothetical*attack_rate_ji

# Lotka-Volterra interaction rate
a_ij <- (-(t(species_id$e*pred_ji)))  # predation
a_ij <- ifelse(a_ij < -2, -2, a_ij)
a_ij <- t(a_ij) 


# Plot 4: Effect of B0 on species
p4 <- ggplot() + 
  geom_line(aes(x = mi, y = -(a_ij[-1, 1])), color = "#89CFF0") + 
  labs(x = "Predator bodysize [log]", y = expression("Effect of B0 on species," * a[i0])) + 
  scale_y_log10() +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = c(0.8, 0.5),
        legend.background = element_rect(fill = "transparent", color = "lightgrey"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank()) + 
  scale_y_log10(breaks = c(10^-4, 10^-3, 10^-2, 10^-1, 10^0),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))
p4

# Plot 5: Intrinsic growth rate
p5 <- ggplot() +
  geom_line(aes(x = mi, y = imu_A_d$mu[-1]), color = "#89CFF0") +
  labs(x = "Predator bodysize [log]", y = "Intrinsic growth rate") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = c(0.8, 0.5),
        legend.background = element_rect(fill = "transparent", color = "lightgrey"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())

p5

a_ij.d <- as.data.frame(a_ij)
a_ij.d

# Plot 6: Invasion into empty environment
p6 <- ggplot() +
  geom_line(aes(x = mi, y = (imu_A_d$mu[-1]) - (a_ij.d[-1, 1])*species_id$K)) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Predator bodysize [log]", y = "Invasion into empty") +
  ylim(min(imu_A_d$mu[-1]), -min(imu_A_d$mu[-1])) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = c(0.8, 0.5),
        legend.background = element_rect(fill = "transparent", color = "lightgrey"),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())

p6

bi <- grid.arrange(bi1, bi2, bi3, bi4, ncol = 2, top = textGrob("Basic interactions", gp = gpar(fontsize = 17, font = 2, face = "bold")))
bi
ggsave("basic_interactions.png", bi, width = 14, height = 12, dpi = 800)
trait <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

ggsave("species_traits.png", trait, width = 14, height = 12, dpi = 800)
###########################################################################################
###########################################################################################

LV_model <- function(t, state, parms) {
  with(as.list(parms), {
    N <- state
    dN <- N * (mu - A %*% N)
    list(dN)
  })
}

density_data <- data.frame()
trophic_data <- data.frame()

for (i in 1:9) {
  n_spec <- 100
  species_id <- generate_species(n_spec, random = TRUE, B0 = 1e-6)
  species_id <- change_temperature(species_id, beta = beta)
  
  mu_A <- compute_LV_param(species_id)
  mu <- mu_A$mu
  A <- mu_A$A
  
  N_0 <- rep(1e2, n_spec)
  N_0[1] <- species_id$K
  N_0_log <- log(N_0)
  t_eval <- seq(0, 100, length.out = 101)

  sol <- ode(y = N_0, times = t_eval, func = LV_model, parms = list(mu = mu, A = A), method = "lsoda")
  
  rel_size <- log(species_id$m_i)
  rel_size <- (rel_size - min(rel_size)) / (max(rel_size) - min(rel_size))
  colors <- viridis(length(rel_size))[floor(rel_size * length(rel_size)) + 1]
  
  time_density <- data.frame(Time = rep(t_eval, each = n_spec), 
                             Species = rep(1:n_spec, times = length(t_eval)), 
                             Density = as.vector(t(sol[, -1])))   
  time_density$Simulation <- i  
  density_data <- rbind(density_data, time_density)
  
  
  surv <- sol[nrow(sol), 2:ncol(sol)] > 1
  trophic_level <- compute_trophic_level(species_id, surv)
  
  temp_trophic <- data.frame(Bodymass = species_id$m_i[surv], 
                             Trophic_Level = trophic_level, 
                             Simulation = i)
  trophic_data <- rbind(trophic_data, temp_trophic)
}


# Population density vs time 
q1 <- ggplot(density_data, aes(x = Time, y = Density, color = factor(Species)), linewidth = 0.8) + 
  geom_line() + 
  viridis::scale_color_viridis(discrete = TRUE) +
  facet_wrap(~ Simulation, ncol = 3) +
  scale_y_log10() +
  labs(x = "Time", y = "Densities") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(
    trans = "log10",  
    limits = c(10^-1, 10^10),  
    breaks = 10^seq(-1, 10, 4),  
    labels = scales::trans_format("log10", scales::math_format(10^.x))  
  ) + 
  scale_x_continuous(breaks = seq(0, 100, 50)) +
  geom_hline(yintercept = 10^7, linetype = "dashed", color = "black", linewidth = 1)  

q1

ggsave("population_density.png", q1, width = 16, height = 13, dpi = 800)

# Trophic Level vs Body Mass
q2 <- ggplot(trophic_data, aes(x = Bodymass, y = Trophic_Level, color = Trophic_Level, size = 8)) +
  geom_point() +
  viridis::scale_color_viridis() +
  facet_wrap(~ Simulation, ncol = 3) +
  scale_x_log10() + 
  scale_y_log10() +
  labs(x = "Bodymass", y = "Trophic level") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        axis.text.y = element_text(size = 11),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(0,4),  
                     breaks = seq(0,4,1)) +  
  scale_x_continuous(trans = "log10",  
                     limits = c(10^-7, 10^5),  
                     breaks = 10^seq(-3, 5, 4),  
                     labels = scales::trans_format("log10", scales::math_format(10^.x)))
q2

#ggsave("trophiclevel.png", q2, width = 14, height = 12, dpi = 800)













