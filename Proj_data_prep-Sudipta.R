#############################################################################################################
# 30211 Introduction to Data Analysis - Project code for data preparation - Sudipta Sarma
# Analysis of 6 (2011-2016) years of consumer complaints data related to financial products/services in USA #
#############################################################################################################
require(scales)
require(utils)

# Read the consumer complaints CSV file retrieved from https://catalog.data.gov/dataset/consumer-complaint-database
consumer.df=read.csv("Consumer_Complaints.csv")
#Summary and structure details of the DF
str(consumer.df)
# All the 18 variables/column names of the consumer complaint dataframe
colnames(consumer.df)
# Response-
# "Date.received"                "Product"                     
# "Sub.product"                  "Issue"                       
# "Sub.issue"                    “Consumer.complaint.narrative"
# "Company.public.response"     "Company"                      
# "State"                       "ZIP.code"                     
# "Tags"                        "Consumer.consent.provided."   
# "Submitted.via"               "Date.sent.to.company"         
# “Company.response.to.consumer"    "Timely.response."             
# "Consumer.disputed."          "Complaint.ID"   

# Convert Date columns to Date format "%m-%d-%Y"
consumer.df$Date.sent.to.company=as.POSIXct(consumer.df$Date.sent.to.company,format="%m-%d-%Y")
consumer.df$Date.received=as.POSIXct(consumer.df$Date.received,format="%m-%d-%Y")
# Removed column - Consumer.complaint.narrative - as it is too big
consumer.df$Consumer.complaint.narrative=NULL

# A look at the different factors and their levels
# 12 types of products and services
levels(consumer.df$Product)

#[1] "Bank account or service" "Consumer Loan"           "Credit card"            
#[4] "Credit reporting"        "Debt collection"         "Money transfers"        
#[7] "Mortgage"                "Other financial service" "Payday loan"            
#[10] "Prepaid card"            "Student loan"            "Virtual currency"  
levels(consumer.df$State)
# [1] ""   "AA" "AE" "AK" "AL" "AP" "AR" "AS" "AZ" "CA" "CO" "CT" "DC" "DE" "FL" "FM" "GA" "GU" "HI"
# [20] "IA" "ID" "IL" "IN" "KS" "KY" "LA" "MA" "MD" "ME" "MH" "MI" "MN" "MO" "MP" "MS" "MT" "NC" "ND"
# [39] "NE" "NH" "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "PR" "PW" "RI" "SC" "SD" "TN" "TX" "UT" "VA"
# [58] "VI" "VT" "WA" "WI" "WV" "WY"
levels(consumer.df$Tags)
# [1] ""                              "Older American"                "Older American, Servicemember"
# [4] "Servicemember"
levels(consumer.df$Submitted.via)
#[1] "Email"       "Fax"         "Phone"       "Postal mail" "Referral"    "Web" 
levels(consumer.df$Company.response.to.consumer)
# [1] "Closed"                          "Closed with explanation"        
# [3] "Closed with monetary relief"     "Closed with non-monetary relief"
# [5] "Closed with relief"              "Closed without relief"          
# [7] "In progress"                     "Untimely response"

# Extract rows to another DF for only CA complaints
consumer.df.CA=consumer.df[consumer.df$State=='CA',]

# Perform string parsing to get ONLY Wells Fargo records in US
consumer.dfAllWells=consumer.df[grep("Wells",consumer.df$Company),]

# Perform string parsing to get ONLY Wells Fargo records in CA
consumer.df.CAWells=consumer.df.CA[grep("Wells",consumer.df.CA$Company),]

nrow(consumer.dfAllWells)
#[1] 47307
nrow(consumer.df.CAWells)
#[1] 9019

# Top six financial companies with the most consumer complaints in US
head(sort(table(consumer.df$Company),decreasing = TRUE))
# Bank of America                  Wells Fargo & Company 
# 60746                                  47307 
# Equifax                   JPMorgan Chase & Co. 
# 40194                                  38116 
# Experian TransUnion Intermediate Holdings, Inc. 
# 38106                                  32256 

# Top six financial companies in CA with the most consumer complaints
head(sort(table(consumer.df.CA$Company),decreasing = TRUE))

# Bank of America Wells Fargo & Company  JPMorgan Chase & Co.              Experian 
# 11664                  9019                  6754                  5520 
# Equifax              Citibank 
# 4878                  4514 

#DF of all complaints in CA for BofA
consumer.df.CABofA=consumer.df.CA[grep("Bank of America",consumer.df.CA$Company),]

# Analysis of Wells Fargo complaint issue types vs  BofA complaint issue types in CA
CAwellsAllissue=head(sort(table(consumer.df.CAWells$Issue),decreasing = TRUE)) # first 6 sorted list of all issue types

# For pie chart calculation
#no. of 'Unsolicited issuance of credit card' issues in CA
unsolCABofAno=nrow(consumer.df.CABofA[grep("Unsolicited",consumer.df.CABofA$Issue),])
unsolCAWellno=nrow(consumer.df.CAWells[grep("Unsolicited",consumer.df.CAWells$Issue),])
acctopclCABofAno=nrow(consumer.df.CABofA[grep("Account open",consumer.df.CABofA$Issue),])
acctopclCAwellno=nrow(consumer.df.CAWells[grep("Account open",consumer.df.CAWells$Issue),])
cancelclCABofAno=nrow(consumer.df.CABofA[grep("Closing",consumer.df.CABofA$Issue),])
cancelclCAwellno=nrow(consumer.df.CAWells[grep("Closing",consumer.df.CAWells$Issue),])

company=c(rep("BofA",3),rep("Wells Fargo",3))

Complaint.Issue=c("Unsolicated credit card","Account open/close/mgmt","Closing/Cancelling account","Unsolicated credit card","Account open/close/mgmt","Closing/Cancelling account")
NoofComplaints=c(unsolCABofAno,acctopclCABofAno,cancelclCABofAno,unsolCAWellno,acctopclCAwellno,cancelclCAwellno)
df1=data.frame(company,Complaint.Issue,NoofComplaints)
# Data for pie chart 1
NoofWellsComplaints=c(CAwellsAllissue[1:6])
# Calculate the percentage labels
comp_labels <- percent(NoofWellsComplaints/sum(NoofWellsComplaints))
# Pie chart lagend labels
lagend_labels=c("Loan mod.,collection,foreclosure","Loan servicing,payments,escrow acct","Acct opening, closing, or mgmt","Deposits and withdrawals","Appln,originator,mort broker","Problems caused by my low funds")

#Compute No. Disputed customers for pie chart 2
consumer.df.CAWellsdisp=consumer.df.CAWells[consumer.df.CAWells$Consumer.disputed=="Yes",]
dispno=nrow(consumer.df.CAWellsdisp) #No. of customers who disputed company response- 2177
undispno=nrow(consumer.df.CAWells)-dispno #No. of customers who do not dispute company respons
# Convert to numeric
totalCAwellsdisputes=as.numeric(c(dispno,undispno))

CAwellsdisputed<- percent(dispno/nrow(consumer.df.CAWells))
CAwellsundisputed<- percent(undispno/nrow(consumer.df.CAWells))
# Pie chart lagend labels
comp_displabels <- c(CAwellsdisputed,CAwellsundisputed)
#[1] "24.1%" - % of consumers who disputed company responses in CA

#For pie chart
lagend_disputelabels=c("Consumers disputed company responses","Consumers did not dispute or NA")

# Vector with 6 states - Top 6 states with most no. of complaints from Wells Fargo
vstateWells = head(sort(table(consumer.dfAllWells$State),decreasing = TRUE))
vAllstateWells = sort(table(consumer.dfAllWells$State),decreasing = TRUE)

# Truncate date received to "%Y/%m" format
short.date = strftime(consumer.df.CAWells$Date.received, "%Y/%m")
#Aggregate records corresponding to column Date Received in "%Y/%m" format
aggr.stat = aggregate(consumer.df.CAWells$Complaint.ID ~ short.date, FUN = NROW)

# Get total number of complaints after aggregation
Aggcomplbymonth=aggr.stat[order(aggr.stat$'consumer.df.CAWells$Complaint.ID',decreasing = TRUE),]
#Top 6 months with most complaints received from Wells Fargo in CA
sixmonthCAWells=head(Aggcomplbymonth)

# 1-sample T-test data computation

# Get the sample mean - No. of wells Fargo complaints in CA
sample_mean=mean(Aggcomplbymonth$`consumer.df.CAWells$Complaint.ID`) #152.8644

allshort.date = strftime(consumer.dfAllWells$Date.received, "%Y/%m")
#Aggregate records corresponding to column Date Received in "%Y/%m" format
allaggr.stat = aggregate(consumer.dfAllWells$Complaint.ID ~ allshort.date, FUN = NROW)
# Get total number of complaints after aggregation
Allaggcomplbymonth=allaggr.stat[order(allaggr.stat$`consumer.dfAllWells$Complaint.ID`,decreasing = TRUE),]
# Compute population mean - Wells Fargo complaints from rest of USA
pop_mean=mean(Allaggcomplbymonth$`consumer.dfAllWells$Complaint.ID`) #801.8136

# Determine p-value and reject Null hypothesis if p < 0.05 - Probability
ttest=t.test(sample_mean,mu=pop_mean)
ttest.pvalue=ttest$p.value

ttest.pvalue <- signif(ttest.pvalue, 3)
# Add significant code based on pvalue < 0.05
ttest.pvalue <- ifelse(ttest.pvalue<0.05, paste0(ttest.pvalue, " ***"), ttest.pvalue)
ttest.pvalue <- paste0("p=", ttest.pvalue)

compbymonthDF=data.frame(Aggcomplbymonth,Allaggcomplbymonth)

# For qqplot to test sample data normality
Wellslocation=c(rep("CA Wells Fargo",59),rep("All Wells Fargo",59))
df=data.frame(Group=Wellslocation,Number=Aggcomplbymonth$`consumer.df.CAWells$Complaint.ID`)
df[60:118,"Number"]=Allaggcomplbymonth$`consumer.dfAllWells$Complaint.ID`

# Wells Fargo complaints with no consent provided by consumers
consumer.df.CAWell.noconsent=consumer.df.CAWells[consumer.df.CAWells$Consumer.consent.provided.=='Consent not provided',c("Product","Sub.product","Issue")]

# Aggregate of CA only complaints by year in CA
byyear= strftime(consumer.df.CAWells$Date.received, "%Y")
CAWells.byyearaggr.stat = aggregate(consumer.df.CAWells$Complaint.ID ~ byyear, FUN = NROW)

BofA.byyear= strftime(consumer.df.CABofA$Date.received, "%Y")
# Aggregate of BofA complaints by year in CA
CABofA.byyearaggr.stat = aggregate(consumer.df.CABofA$Complaint.ID ~ BofA.byyear, FUN = NROW)

# Simple Linear regression model
CAWells.byyearaggr.stat$byyear=as.POSIXct(CAWells.byyearaggr.stat$byyear,format="%Y")
complaint.model=lm(`consumer.df.CAWells$Complaint.ID` ~ byyear,data=CAWells.byyearaggr.stat)
summary(complaint.model)