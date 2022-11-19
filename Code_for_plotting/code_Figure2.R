# load library
library(ggplot2)
library(ggthemes)

# define label for each panel
f_labels <- data.frame(Cause = c("All-cause","Cardiovascular","Respiratory"), 
                       label = c("All-cause","Cardiovascular","Respiratory"),
                       y_lable=c(1.029,1.025,1.047))

# line plot
p_dlnm<-
  ggplot(data_fig2, aes(x = Lag_day, y = RR_estimate)) +
  geom_ribbon(aes(ymin = RR_lower, ymax = RR_upper), alpha = 0.2) +
  geom_line(size = 0.3)+
  geom_hline(yintercept =  1 )+
  theme_few() +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14),labels = c("0","2","4","6","8","10","12","14")) +
  facet_wrap(~Cause, scales = "free_y", ncol = 1)+
  geom_text(aes(label = label, x = 7, y = y_lable), data = f_labels)+
  ylab("Estimated effects") + xlab("Lag days") +
  theme(
    strip.background =element_rect(fill="white"),
    strip.text = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(size = rel(1.0),colour = "black"),
    axis.text.y = element_text(size = rel(1.0), colour = "black"),
    axis.title.x = element_text(size = rel(1.2), colour = "black"),
    axis.title.y = element_text(size = rel(1.2), colour = "black"))

# output figure
path = "~/Figures/"

# save as PDF file
pdf(paste(path,"Figure_2.pdf",sep = ""),width =6,height = 9)
p_dlnm
dev.off()