setwd("/home/georgehagstrom/work/Teaching/DATA607/website/meetups/Meetup2/")
gopnik <- read_csv("gopnik.txt")

gopnikData = tibble(Age_Category = character(),
                    Condition = character(),
                    Accuracy = numeric())


gopnikData = gopnikData |> add_row(Age_Category = "Child", Condition = "Single", Accuracy = 1.0)
gopnikData = gopnikData |> add_row(Age_Category = "Adult", Condition = "Single", Accuracy = 23.0/28.0)
gopnikData = gopnikData |> add_row(Age_Category = "Child", Condition = "Multiple", Accuracy = 21.0/25.0)
gopnikData = gopnikData |> add_row(Age_Category = "Adult", Condition = "Multiple", Accuracy = 10/28.0)


gopnik$se = 0

for (i in 1:4){
  
  gopnik$se[i] = sqrt( (gopnik$N_single[i] + 2)*(gopnik$N_multiple[i] + 2) / (gopnik$N_multiple[i]+gopnik$N_single[i] + 4)^3)     
  
}

gopnikData =  gopnikData |> mutate(se = gopnik$se[c(1, 3, 2, 4)])


gopnikLabel = gopnikData |> group_by(Age_Category) |> summarise(meanAccuracy = mean(Accuracy))
gopnikLabel$Condition = 0.5



gopnikData = read_csv("gopnikData,csv")

gopnikData |> mutate(numberCondition = ifelse(Condition == "Single",0,1)) |> 
  ggplot(aes(x=numberCondition,y=Accuracy,color=Age_Category)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  geom_pointrange(aes(ymin=Accuracy - se, ymax=Accuracy*( (Accuracy + se) > 1  ) + (Accuracy + se) * ((Accuracy + se) < 1  )),
                  show.legend = FALSE) +
  scale_y_continuous(limit = c(0,1.01),breaks = seq(0,1,by=0.5)) +
  scale_x_continuous(limit = c(0,1),
                     breaks = seq(0,1,by=1),
                     labels = c("Single","Multiple")) +
  theme_bw(base_size = 18,) +
  xlab("Experiment Condition") +
  ylab("Fraction Who Answer Correctly") +
  labs(color="Age",title = "Adults Versus Children Accuracy in a Selection Task") +
  geom_text_repel(data = gopnikLabel,aes(x=Condition,y=meanAccuracy,label=Age_Category,color=Age_Category),
                  show.legend = FALSE,
                  size = 8)
 
ggsave("ImprovedGopnik.svg")

gopnik$se = 0

for (i in 1:4){
  
  gopnik$se[i] = sqrt( (gopnik$N_single[i] + 2)*(gopnik$N_multiple[i] + 2) / (gopnik$N_multiple[i]+gopnik$N_single[i] + 4)^3)     
  
}

gopnikData |> mutate(se = gopnik$se[c(1, 3, 2, 4)])

gopnikData = gopnikData |> mutate(numberCondition = ifelse(Condition == "Single",0,1))

gopnikData |> 
  ggplot(aes(x = numberCondition,y = Accuracy)) +
  geom_line((aes(group = Age_Category,color = Age_Category))) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = Age_Category), size = 2) +
  geom_text(
    data = gopnikData |> filter(Condition == "Multiple"),
    aes(x = numberCondition + 0.1, y = Accuracy, label = Age_Category),
    family = "Myriad Pro",
    size = 10/.pt,
    hjust = 0) +
  scale_x_continuous(
    limits = c(0, 1.1), breaks = c(0, 1),
    labels = c("Single", "Multiple"),
    expand = expansion(add = c(0.1, 0.1)),
    name = NULL,
    position = "top"
  ) +
  scale_y_continuous(
    limits = c(0, 1.0),
    expand = expansion(add = c(0,.05)),
    name = parse(text = "Accuracy")
  ) +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(),
    axis.text.y = element_text(),
    axis.line.y.left = element_line(),
    axis.text.y.right = element_text(
      hjust = 0, vjust = .5,
      margin = margin(0, 0, 0, 0),
      color = "black",
      lineheight = 0.8
    ),
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    plot.margin = margin(35, 50, 70, 15)
  )
  

N <- gopnik$N_single + gopnik$N_multiple
p_multiple <- gopnik$N_multiple / N
p_correct <- ifelse(gopnik$Combination==0, 1 - p_multiple, p_multiple)
colors <- c("red", "blue")
combination_labels <- c("Individual\ncondition", "Combination\ncondition")
adult_labels <- c("Children", "Adults")

pdf("gopnik_2.pdf", height=4, width=5)
par(mar=c(3,3,3,2), mgp=c(1.7, .5, 0), tck=-.01, bg="gray90")
plot(c(0,1), c(0,1), yaxs="i", xlab="", ylab="Percent who gave correct answer", xaxt="n", yaxt="n", type="n", main="Children did better than adults,\nespecially in the combination condition", cex.main=.9)
axis(1, c(0, 1), combination_labels, mgp=c(1.5,1.5,0))
axis(2, c(0,.5,1), c("0", "50%", "100%"))



for (i in 1:2){
  ok <- gopnik$Adult==(i-1)
  x <- gopnik$Combination[ok]
  y <- p_correct[ok]
  se <- sqrt((N*p_correct + 2)*(N[ok]*(1-p_correct) + 2)/(N[ok] + 4)^3)
  lines(x, y, col=colors[i])
  points(x, y, col=colors[i], pch=20)
  for (j in 1:2){
    lines(rep(x[j], 2), y[j] + se[j]*c(-1,1), lwd=2, col=colors[i])
    lines(rep(x[j], 2), y[j] + se[j]*c(-2,2), lwd=.5, col=colors[i])
  }
  text(mean(x), mean(y) - .05, adult_labels[i], col=colors[i], cex=.9)
}
dev.off()
