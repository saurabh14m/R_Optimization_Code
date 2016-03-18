require(financial);  
require(psych);
require(xlsx); 
require(RPostgreSQL);
require(sqldf);
options(sqldf.driver = "SQLite")

install.packages("RPostgreSQL")
require("RPostgreSQL")
install.packages("dplyr")
require("dplyr")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {  "password" }

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Rio",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password


df_Scenario_1 <- dbGetQuery(con, 'SELECT * from \"Scenario\" ')
# colnames(df_Scenario_1[1]) -> "senario_id"
names(df_Scenario_1)[names(df_Scenario_1) == 'Scenario_ID'] <- 'senario_id'
names(df_Scenario_1)[names(df_Scenario_1) == 'Project_ID'] <- 'project_id'
scenario_unique<- distinct(df_Scenario_1[c("project_id","senario_id","Total_Project_Capex")])
scenario_unique_1 <- within(scenario_unique,Total_Project_Capex<-(-Total_Project_Capex)/50)
my_df_2 <- merge(my_df_2,scenario_unique_1,by=c('project_id', 'senario_id'))
#my_df_2 <- merge(my_df_2,scenario_unique_1,by=c("Project_ID","senario_id"))
my_df_2 <- within(my_df_2,PI<- NPV/Total_Project_Capex)


project_id <- .arg2 ;
senario_id <- .arg3 ;
year_end <- .arg1 ;
year_id <- .arg4 ;
year_end_discounted <- .arg5 ;
Revenue <- .arg7 ; 
Risk <- .arg6 ;
Discount_rate <- .arg8 ;
Priority <- .arg9 ;
Commodity <- .arg10

my_df <- data.frame(project_id ,senario_id , year_end , year_id ,year_end_discounted, paste(project_id,senario_id) , Revenue, Risk , Discount_rate, Priority, Commodity);
my_df <- subset(my_df , my_df$year_id>2015);
my_df <- my_df[with(my_df, order(project_id, senario_id, year_id)), ]

#write.xlsx(my_df, "MyData_In.xlsx")
my_df = read.xlsx("MyData_In.xlsx",1)

out <- split( my_df , f = my_df$paste.project_id..senario_id.)
irr <- NULL

for( i in 1:length(out))
{
year_end_list = out[[i]]$year_end
year_end_discounted_list = out[[i]]$year_end_discounted
y = cf(year_end_list)
out[[i]]$irr =max(y$irr)
out[[i]]$NPV =sum(year_end_discounted_list)
} 

my_df_new <- unsplit(out, f = my_df$paste.project_id..senario_id.)
write.xlsx(my_df_new, "MyData.xlsx")

if (levels(my_df_new$Priority) =="1") { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
having max(NPV)      
order by NPV desc, project_id ,year_id
")
my_df_2 = my_df_1[order(my_df_1$NPV , decreasing = TRUE),][1:250,]

} else if  (levels(my_df_new$Priority) =="2") { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
having max( irr)
order by irr desc, project_id ,year_id
")
my_df_2 =my_df_1[order(my_df_1$irr, decreasing = TRUE),][1:250,]

} else if  (levels(my_df_new$Priority) =="3") { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
having max(Revenue)
order by Revenue desc, project_id ,year_id
")
my_df_2 =my_df_1[order(my_df_1$Revenue, decreasing = TRUE),][1:250,]
} else { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
having max(NPV)       ")
}

my_df1a = sqldf("select * from my_df_new group by project_id ,senario_id having max(NPV) ")

lprec <- make.lp(0, 3)
library(lpSolveAPI)

set.objfn(lprec, my_df1a$irr[1:3])




obj <-my_df1a$irr[1:3]
mat <- matrix(c(my_df1a$year_end[1:3] ,1, 0, 0) , nrow = 3)
mat <- t(mat)

dir <- c("<=", "<=")
rhs <- c(-50000000, 1)
max <- TRUE

Rglpk_solve_LP(obj, mat, dir, rhs, max = max, control = list("verbose" = 
                                                               TRUE, "canonicalize_status" = FALSE))






#write.xlsx(my_df_2, "Prioritized.xlsx")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {  "password" }

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# Disconnect a previous connection
dbDisconnect(con) 

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Rio",
host = "localhost", port = 5432,
user = "postgres", password = pw)
rm(pw) # removes the password

dbExistsTable(con, "Scenario")
# TRUE
# query the data from postgreSQL 
df_Commodities <- dbGetQuery(con, "SELECT * from \"Commodities\" ")

dbGetQuery(con, "drop table  \"Prioritized\" ")

dbWriteTable(con, "Prioritized", my_df_2, row.names=FALSE)
dbDisconnect(con) 

my_df_new$irr





' 
, ([Year_End_Balance_Dynamic]) //.arg1
, ATTR([Project ID]) //.arg2
, ATTR([Scenario ID (Phases)]) //.arg3
,MIN([Year])//.arg4
,[Year_End_Balance_Discounted]//.arg5
,MIN([Country Risk Premium])//.arg6
,[Revenue] //.arg7
,AVG([Discount Rate]) //.arg8
,[Priority List] //.arg9
,ATTR([Commodity]) //.arg10