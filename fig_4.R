library(ggplot2)
library(gridExtra)
# giving n, t, alpha and Beta
n <- 10
t <- 2
alpha <- 10
beta <- 10
NNN <- 100 # we consider NNN=10000 in the paper
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta0 <- -log(1 - gtheta)
mean(theta0)

alpha <- 1
beta <- 1
NNN <- 100
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta <- -log(1 - gtheta)
mean(theta)
c(mean(theta0), mean(theta))

alpha <- 0.1
beta <- 0.1
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta1 <- -log(1 - gtheta)
mean(theta1)

alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta2 <- -log(1 - gtheta)
mean(theta2)

alpha <- 0.001
beta <- 0.001
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta3 <- -log(1 - gtheta)
mean(theta3)
alpha <- 0.0001
beta <- 0.0001
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta4 <- -log(1 - gtheta)
mean(theta4)

df <- data.frame(
  Hyper_parameters = factor(rep(c("alpha=beta=1", "alpha=beta=0.1", "alpha=beta=0.01", "alpha=beta=0.001"), each = NNN)),
  value = c(theta, theta1, theta2, theta3)
)
df <- data.frame(
  Mean = c(mean(theta0), mean(theta), mean(theta1), mean(theta2), mean(theta3), mean(theta4)),
  sd = c(sd(theta0), sd(theta), sd(theta1), sd(theta2), sd(theta3), sd(theta4)),
  l = c(quantile(theta0, 0.025), quantile(theta, 0.025), quantile(theta1, 0.025), quantile(theta2, 0.025), quantile(theta3, 0.025), quantile(theta4, 0.025)),
  u = c(quantile(theta0, 0.975), quantile(theta, 0.975), quantile(theta1, 0.975), quantile(theta2, 0.975), quantile(theta3, 0.975), quantile(theta4, 0.975)),
  k = c("-1", "0", "1", "2", "3", "4")
)
p1 <- ggplot(df, aes(x = k, y = Mean)) +
  ggtitle("(d)") +
  geom_line(aes(group = 1), color = "lightpink", lwd = 1.1) +
  geom_errorbar(aes(ymin = l, ymax = u), width = 0.2, color = "lightskyblue", lwd = 1.1) +
  geom_point(size = 2) +
  xlab("k") +
  ylab("Posterior mean")
 
n <- 10
t <- 2
alpha <- 1
beta <- 1
MLE <- -log(1 - t / n)
NNN <- 100
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta <- -log(1 - gtheta)
mean(theta)

alpha <- 0.1
beta <- 0.1
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta1 <- -log(1 - gtheta)
mean(theta1)

alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta2 <- -log(1 - gtheta)
mean(theta2)

alpha <- 0.001
beta <- 0.001
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta3 <- -log(1 - gtheta)
mean(theta3)

df <- data.frame(
  Hyper_parameters = factor(rep(c("alpha=beta=1", "alpha=beta=0.1", "alpha=beta=0.01", "alpha=beta=0.001"), each = NNN)),
  value = c(theta, theta1, theta2, theta3)
)
p2 <- ggplot(df, aes(x = value, fill = Hyper_parameters)) +
  ggtitle("(a)") +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = MLE, size = 0.7, color = "red", lty = 2) +
  geom_text(aes(x = MLE + .13, label = paste0("MLE=0.223"), y = 3)) +
  labs(fill = "") +
  scale_fill_discrete(labels = c(
    expression(alpha ~ "," ~ beta ~ " = 0.001"),
    expression(alpha ~ "," ~ beta ~ " = 0.01"),
    expression(alpha ~ "," ~ beta ~ " = 0.1"),
    expression(alpha ~ "," ~ beta ~ " =1")
  ))

n <- 10
t <- n / 2
alpha <- 0.01
beta <- 0.01
NNN <- 100
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta <- -log(1 - gtheta)
mean(theta)

n <- 20
t <- n / 2
alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta1 <- -log(1 - gtheta)
mean(theta1)

n <- 50
t <- n / 2
alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta2 <- -log(1 - gtheta)
mean(theta2)

n <- 100
t <- n / 2
alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta3 <- -log(1 - gtheta)
mean(theta3)

df <- data.frame(
  Hyper_parameters = factor(rep(c(
    "n=10", "n=20",
    "n=50", "n=100"
  ), each = NNN)),
  value = c(theta, theta1, theta2, theta3)
)
p3 <- ggplot(df, aes(x = value, fill = Hyper_parameters)) +
  ggtitle("(b)") +
  geom_density(alpha = 0.3) +
  labs(fill = "") +
  scale_fill_discrete(labels = c(
    expression(~n ~ " = 100"),
    expression(n ~ " = 50"),
    expression(n ~ " = 20"),
    expression(n ~ " =10")
  ))

n <- 10
t <- n
alpha <- 0.01
beta <- 0.01
NNN <- 100
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta <- -log(1 - gtheta)
mean(theta)

n <- 100
t <- n
alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta1 <- -log(1 - gtheta)
mean(theta1)

n <- 1000
t <- n
alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta2 <- -log(1 - gtheta)
mean(theta2)

n <- 5000
t <- n
alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta3 <- -log(1 - gtheta)
mean(theta3)

n <- 10000
t <- n
alpha <- 0.01
beta <- 0.01
set.seed(1)
gtheta <- rbeta(NNN, alpha + t, n + beta - t + 1)
theta4 <- -log(1 - gtheta)
mean(theta4)

df <- data.frame(
  Hyper_parameters = factor(rep(c(
    "n=10", "n=100",
    "n=1000", "n=5000", "n=10000"
  ), each = NNN)),
  value = c(theta, theta1, theta2, theta3, theta4)
)
p4 <- ggplot(df, aes(x = value, fill = Hyper_parameters)) +
  geom_density(alpha = 0.3) +
  ggtitle("(c)") +
  labs(fill = "") +
  scale_fill_discrete(labels = c(
    expression(~n ~ " = 10000"),
    expression(~n ~ " = 5000"),
    expression(n ~ " = 1000"),
    expression(n ~ " = 100"),
    expression(n ~ " =10")
  ))
grid.arrange(p2, p3, p4, p1,
  ncol = 2, nrow = 2
)