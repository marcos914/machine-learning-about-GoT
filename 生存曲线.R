memory.limit()
###seminar-survival
###rawdata
####Rzidai
install.packages("survival")
library(survival)
Lung <- lung
View(Lung)
###kaplan-meier曲线
####基本流程：Surv()、survfit()、survdiff()
Lusurv <-Surv(time = Lung$time,event = Lung$status)
Lufit <- survfit(Lusurv~Lung$sex)
plot(Lufit,conf.int = 'none',
     col=c('#C51B7D','#4D9221'),lwd=2,mark.time = T,xlab = 'Time',ylab = 'Survival Probality')
survdiff(Lusurv~Lung$sex)
abline(h = 0.5)
abline(v=270,lty = 3)
abline(v=426,lty = 3)
legend('bottomleft',c('male','female'),
       col=c('#C51B7D','#4D9221'),lwd=2)
text(900,0.9,'p=0.001')

  