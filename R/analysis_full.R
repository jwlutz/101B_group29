params <-
list(show_code = TRUE)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 6, fig.height = 3.2, fig.align = "center")
library(ggplot2); library(pwr); library(knitr); library(car)

navy <- "#1E2761"; terra <- "#B85042"; ink <- "#1A1A1A"; gray <- "#6B7280"

d <- read.csv("../data/between_subjects.csv", stringsAsFactors = FALSE)
d$baseline   <- ifelse(d$difficulty == "Easy", d$baseline_easy, d$baseline_hard)
stopifnot(all(d$baseline - d$response == d$diff))   # data integrity check
d$difficulty <- factor(d$difficulty, levels = c("Easy", "Hard"))
d$dose       <- factor(d$shots, levels = c(1, 2, 3))
d$shots_n    <- d$shots

# --- change-score models (primary) ----------------------------------------
fit     <- aov(diff ~ difficulty * dose, data = d)    # block * treatment
fit_lin <- lm(diff ~ difficulty * shots_n, data = d)  # dose as ordinal trend
an      <- summary(fit)[[1]]
getp <- function(t) signif(an[trimws(rownames(an)) == t, "Pr(>F)"], 2)
getF <- function(t) round(an[trimws(rownames(an)) == t, "F value"], 2)
sl_easy <- coef(lm(diff ~ shots_n, data = subset(d, difficulty == "Easy")))[2]
sl_hard <- summary(lm(diff ~ shots_n, data = subset(d, difficulty == "Hard")))$coef

# --- ANCOVA models (robustness) --------------------------------------------
anc     <- lm(response ~ baseline + difficulty * dose, data = d)
anc_an  <- Anova(anc, type = 2)
anc_lin <- lm(response ~ baseline + difficulty * shots_n, data = d)
b_base  <- coef(anc)["baseline"]
sd_diff <- sigma(fit); sd_anc <- sigma(anc)
anc_dose_p <- anc_an["dose", "Pr(>F)"]
anc_int_p  <- summary(anc_lin)$coef["difficultyHard:shots_n", "Pr(>|t|)"]
hslope_anc <- summary(lm(response ~ baseline + shots_n,
                         data = subset(d, difficulty == "Hard")))$coef["shots_n", ]
slopes_p   <- Anova(lm(response ~ baseline * dose + baseline * difficulty +
                       difficulty * dose, data = d), type = 2)["baseline:dose", "Pr(>F)"]

# --- effect size + power ---------------------------------------------------
ss <- an[, "Sum Sq"]; names(ss) <- trimws(rownames(an))
f_dose <- sqrt((ss["dose"]/(ss["dose"]+ss["Residuals"])) /
               (1 - ss["dose"]/(ss["dose"]+ss["Residuals"])))
n_large <- pwr.anova.test(k = 3, f = 0.40, sig.level = 0.05, power = 0.80)$n
n_obs   <- pwr.anova.test(k = 3, f = as.numeric(f_dose), sig.level = 0.05, power = 0.80)$n
pow_20  <- pwr.anova.test(k = 3, n = 20, f = 0.40, sig.level = 0.05)$power

# --- helpers to print clean tables in both PDF and Word --------------------
fmt_anova <- function(tab) {
  a <- as.data.frame(tab)
  pcol <- grep("Pr", names(a))
  num  <- setdiff(seq_along(a), pcol)
  a[num] <- lapply(a[num], function(x) ifelse(is.na(x), NA, round(x, 2)))
  a[[pcol]] <- ifelse(is.na(a[[pcol]]), "", formatC(a[[pcol]], format = "f", digits = 4))
  a
}


## ----power, echo=params$show_code---------------------------------------------
pwr.anova.test(k = 3, f = 0.40, sig.level = 0.05, power = 0.80)$n


## ----celltable----------------------------------------------------------------
cm <- tapply(d$diff, list(d$difficulty, d$dose), mean)
tab <- as.data.frame(round(cm, 2)); colnames(tab) <- c("1 shot", "2 shots", "3 shots")
kable(tab, caption = "Mean drop in score (sober baseline minus post-treatment) by difficulty block and dose. Each cell is 10 islanders.")


## ----interaction, fig.height=2.9, fig.cap="Mean drop in arithmetic score (with one-standard-error bars) by alcohol dose, for Easy and Hard tasks. Alcohol degrades hard-task performance in a dose-dependent way; easy-task performance is essentially unaffected."----
agg <- aggregate(diff ~ difficulty + dose, d,
                 function(v) c(m = mean(v), se = sd(v)/sqrt(length(v))))
agg <- do.call(data.frame, agg); names(agg)[3:4] <- c("m", "se")
ggplot(agg, aes(dose, m, color = difficulty, group = difficulty)) +
  geom_line(linewidth = 1.2) +
  geom_errorbar(aes(ymin = m - se, ymax = m + se), width = .08, linewidth = .8) +
  geom_point(size = 3) +
  scale_color_manual(values = c(Easy = navy, Hard = terra), name = "Task") +
  scale_x_discrete(labels = c("1 shot", "2 shots", "3 shots")) +
  labs(x = "Alcohol dose", y = "Mean drop in score") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", color = ink))


## ----anovacode, echo=params$show_code-----------------------------------------
fit <- aov(diff ~ difficulty * dose, data = d)   # block * treatment, change score

## ----anovatab-----------------------------------------------------------------
kable(fmt_anova(summary(fit)[[1]]),
      caption = "Change-score ANOVA. Difficulty is the blocking factor, dose the treatment.")


## ----trend, echo=params$show_code---------------------------------------------
fit_lin <- lm(diff ~ difficulty * shots_n, data = d)  # dose as a numeric trend


## ----ancovacode, echo=params$show_code----------------------------------------
anc <- lm(response ~ baseline + difficulty * dose, data = d)  # ANCOVA

## ----ancovatab----------------------------------------------------------------
kable(fmt_anova(Anova(anc, type = 2)),
      caption = "ANCOVA (Type II): post-treatment score adjusted for the sober baseline.")


## ----tukey, echo=params$show_code---------------------------------------------
TukeyHSD(fit, "dose")$dose   # pairwise dose comparisons (change score)


## ----diag, fig.height=2.7, fig.cap="Diagnostic plots for the change-score model: residual Q-Q and residuals vs. fitted."----
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
qqnorm(residuals(fit), main = "Normal Q-Q", pch = 19, col = navy, cex = .6)
qqline(residuals(fit), col = terra, lwd = 2)
plot(fitted(fit), residuals(fit), main = "Residuals vs Fitted",
     xlab = "Fitted", ylab = "Residual", pch = 19, col = navy, cex = .6)
abline(h = 0, lty = 2, col = gray)

