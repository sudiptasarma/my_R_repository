#############################################################################################################
# 30211 Introduction to Data Analysis - Project code for Graphics - Sudipta Sarma
# Analysis of 6 (2011-2016) years of consumer complaints data related to financial products/services in USA #
#############################################################################################################

library(ggplot2)
require(graphics)
library(calibrate)
## Line chart ##

# Calculate range from 0 to max value of 2 vectors
g_range<-range(100, as.vector(CABofA.byyearaggr.stat$`consumer.df.CABofA$Complaint.ID`), as.vector(byyearaggr.stat$`consumer.df.CAWells$Complaint.ID`))

# Graph consumer complaints using y axis that ranges from 0 to max 
# value in BofA or Wells Fargo vector.  Turn off axes and 
# annotations (axis labels) so we can specify them ourself
plot(as.vector(CABofA.byyearaggr.stat$`consumer.df.CABofA$Complaint.ID`), type="o", col="blue", axes=FALSE, ann=FALSE)

# Make x axis using 2011-2016 labels
axis(1, at=1:6, lab=c(2011:2016))

# Make y axis with horizontal labels
axis(2, las=1, at=c(1000,1500,2000,2500,3000,3500))

# Create box around plot
box()

# Graph Wells Fargo complaints with red dashed line and square points
lines(as.vector(byyearaggr.stat$`consumer.df.CAWells$Complaint.ID`), type="o", pch=22, col="red")

# Create a title with a red, bold/italic font
title(main="Wells Fargo vs. Bank of America complaints in CA in 6 years", col.main="blue", font.main=4)

# Label the x and y axes with dark green text
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="Total", col.lab=rgb(0,0.5,0))

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots 
legend(4, g_range[2], c("Bank of America","Wells Fargo"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2)

grid()

#Bar chart -Top 6 states with Wells Fargo Complaints
## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.3*max(vstateWells))
## Plot, and store x-coordinates of bars in stateplot
stateplot=barplot(vstateWells, xlab="States",  ylab="Total",col=rainbow(10), border="blue",ylim = ylim, density=c(10,20,30,40,50,60))
## Add text at top of bars
text(x = stateplot, y = vstateWells, label = vstateWells, pos = 3, cex = 0.8, col = "red")
box()
title(main="Top 6 states with Wells Fargo Complaints", col.main="red", font.main=4)
grid()

## Find a range of y's that'll leave sufficient space above the tallest bar
ylim <- c(0, 1.2*max(sixmonthCAWells$`consumer.df.CAWells$Complaint.ID`))
## Plot, and store x-coordinates of bars in stateplot
sixmonthplot=barplot(sixmonthCAWells$`consumer.df.CAWells$Complaint.ID`, xlab="Month/Year",  ylab="Total Complaints", col=rainbow(10) ,ylim = ylim, density=c(10,20,30,40,50,60))


text(x=sixmonthplot, y=-2, sixmonthCAWells$short.date, cex=0.8, srt=45, xpd=TRUE)

## Add text at top of bars
text(x = sixmonthplot, y = sixmonthCAWells$`consumer.df.CAWells$Complaint.ID`, label = sixmonthCAWells$`consumer.df.CAWells$Complaint.ID`, pos = 3, cex = 0.8, col = "blue")
box()
title(main="Top 6 months with most no. of Wells Fargo Complaints in CA", col.main="blue", font.main=4)
grid()

# Pie chart
pie(NoofWellsComplaints, main="Distribution of Wells Fargo Complaint Issue Types in CA", col=rainbow(10), labels=comp_labels,cex=0.8)

# Create a legend at the right   
legend(1.3, 0.5, lagend_labels, cex=0.6, fill = rainbow(10))

#Pie chart for consumer disputed company response or not
pie(totalCAwellsdisputes, main="Percentage Distribution of Wells Fargo consumer disputes in CA", col=rainbow(10), labels=comp_displabels,cex=0.8)

# Create a legend at the right   
legend(1.3, 0.5, lagend_disputelabels, cex=0.8, fill = rainbow(10))

# Plot the grouped multi-bar plot to compare BofA vs Wells Fargo complaints in 3 issue types in CA
p=ggplot(df1, aes(Complaint.Issue, NoofComplaints, fill = company)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1")

p <- p + labs( x="Complaint Issue types", y="No. of Complaints", title = "Wells Fargo vs Bank of America complaint issue types in CA")
p + theme(plot.title = element_text(size = rel(2), colour = "blue"))

# Q-Q plot
qqp=ggplot(df, aes(sample = Number, group = Group, color = Group)) +
  annotate("text",x=0.30*1,y=0.45*1,label=ttest.pvalue) +
  stat_qq()

qqp <- qqp + labs( x="", y="No. of Complaints", title = "Wells Fargo in CA vs Wells Fargo in the rest of US Q-Q plot")
qqp + theme(plot.title = element_text(size = rel(2), colour = "blue"))

# Plot complaint model
plot(complaint.model)
abline(complaint.model,col='red')
grid()

title(main="Complaint model with year as predictor and complaint count as response variable", col.main="blue", font.main=4)

x=CAWells.byyearaggr.stat$byyear
y=CAWells.byyearaggr.stat$`consumer.df.CAWells$Complaint.ID`
mod1=complaint.model

plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10),xlab="Years",ylab="Total", col.lab=rgb(0,0.5,0))
abline(mod1, lwd=2,col="green")

# calculate residuals and predicted values
res <- signif(residuals(mod1), 5)
pre <- predict(mod1) # plot distances between points and the regression line
segments(x, y, x, pre, col="red")

# add labels (res values) to points

textxy(x, y, res, cx=0.7)
title(main="Complaint model with 'year' as predictor and 'complaint count' as response variable", col.main="blue", font.main=4)