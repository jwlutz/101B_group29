# Figures for the Group 29 deck (expanded 13-block dataset).
# Palette matches the slides: navy 1E2761, terracotta B85042, ice CADCFC.
suppressMessages(library(ggplot2))

navy <- "#1E2761"; terra <- "#B85042"; ice <- "#CADCFC"
gray <- "#6B7280"; lgray <- "#DDDDDD"; ink <- "#1A1A1A"

cols <- c("subject", "alcohol", "difficulty", "score")
raw <- rbind(read.csv("../data/scores.csv", stringsAsFactors = FALSE)[, cols],
             read.csv("../data/new_subjects.csv", stringsAsFactors = FALSE)[, cols])
raw$alcohol    <- factor(raw$alcohol, levels = c(0, 1, 3))
raw$difficulty <- factor(raw$difficulty, levels = c("Easy", "Hard"))
raw$subject    <- factor(raw$subject)
complete <- names(which(table(raw$subject) == 6))
dat <- droplevels(raw[raw$subject %in% complete, ])

fit <- aov(score ~ alcohol * difficulty + subject, data = dat)

base_theme <- theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(color = ink, face = "bold"),
        axis.text = element_text(color = gray),
        legend.position = "top",
        legend.title = element_text(color = ink, face = "bold"),
        plot.margin = margin(10, 14, 8, 8))

dir.create("figs", showWarnings = FALSE)

## 1. Interaction plot: means +/- SE -----------------------------------------
agg <- aggregate(score ~ alcohol + difficulty, dat, function(v)
  c(m = mean(v), se = sd(v) / sqrt(length(v))))
agg <- do.call(data.frame, agg)
names(agg)[3:4] <- c("mean", "se")

p1 <- ggplot(agg, aes(alcohol, mean, color = difficulty, group = difficulty)) +
  geom_line(linewidth = 1.3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.08, linewidth = 0.9) +
  geom_point(size = 3.6) +
  scale_color_manual(values = c(Easy = navy, Hard = terra), name = "Task") +
  scale_x_discrete(labels = c("0 shots", "1 shot", "3 shots")) +
  labs(x = "Alcohol dose", y = "Mean correct (of 40)") +
  base_theme
ggsave("figs/interaction.png", p1, width = 6.2, height = 4.5, dpi = 220, bg = "white")

## 2. Tukey HSD on alcohol: forest plot --------------------------------------
tk <- as.data.frame(TukeyHSD(fit, "alcohol")$alcohol)
tk$comp <- rownames(tk)
tk$comp <- factor(tk$comp, levels = c("1-0", "3-1", "3-0"),
                  labels = c("1 vs 0 shot", "3 vs 1 shot", "3 vs 0 shot"))
tk$lab <- ifelse(tk$`p adj` < 0.001, "p < 0.001", sprintf("p = %.3f", tk$`p adj`))

p2 <- ggplot(tk, aes(diff, comp)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = gray) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.16, linewidth = 1, color = navy) +
  geom_point(size = 4, color = terra) +
  geom_text(aes(label = lab), vjust = -1.1, color = ink, size = 4.2) +
  labs(x = "Difference in mean correct (95% CI)", y = NULL) +
  base_theme +
  theme(panel.grid.major.y = element_blank())
ggsave("figs/tukey.png", p2, width = 6.2, height = 3.4, dpi = 220, bg = "white")

## 3. Diagnostics: residual QQ + residuals vs fitted ------------------------
res <- residuals(fit)
fit_v <- fitted(fit)
sw <- shapiro.test(res)

qq <- qqnorm(res, plot.it = FALSE)
dq <- data.frame(theo = qq$x, samp = qq$y)
sl <- diff(quantile(res, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
ic <- median(res) - sl * median(dq$theo)

p3 <- ggplot(dq, aes(theo, samp)) +
  geom_abline(slope = sl, intercept = ic, color = terra, linewidth = 1) +
  geom_point(color = navy, size = 2, alpha = 0.8) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles",
       subtitle = sprintf("Normal Q-Q  (Shapiro-Wilk p = %.2f)", sw$p.value)) +
  base_theme + theme(plot.subtitle = element_text(color = ink, face = "bold"))
ggsave("figs/qq.png", p3, width = 6.0, height = 4.4, dpi = 220, bg = "white")

p4 <- ggplot(data.frame(fit_v, res), aes(fit_v, res)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = gray) +
  geom_point(color = navy, size = 2, alpha = 0.8) +
  labs(x = "Fitted value", y = "Residual", subtitle = "Residuals vs fitted") +
  base_theme + theme(plot.subtitle = element_text(color = ink, face = "bold"))
ggsave("figs/resid_fitted.png", p4, width = 6.0, height = 4.4, dpi = 220, bg = "white")

cat("wrote interaction.png, tukey.png, qq.png, resid_fitted.png\n")
