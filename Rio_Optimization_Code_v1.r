
my_df <- data.frame(project_id ,senario_id , year_end , year_id ,year_end_discounted, paste(project_id,senario_id) , Revenue, Risk , Discount_rate, Priority, Commodity, Total_Capex);
write.xlsx(my_df, "MyData_In.xlsx")
#my_df <- subset(my_df , my_df$year_id>2015);
my_df <- subset(my_df , my_df$year_id>2015  & my_df$year_id<=time_horizon);

my_df <- my_df[with(my_df, order(project_id, senario_id, year_id)), ]

#write.xlsx(my_df, "MyData_In.xlsx")
names(my_df)[6]="combined_id"

out <- split( my_df , f = my_df$combined_id)
#out <- split( my_df , f = my_df$paste.project_id..senario_id.)

irr <- NULL
 
for( i in 1:length(out))
{
  year_end_list = out[[i]]$year_end
  year_end_discounted_list = out[[i]]$year_end_discounted
  y = cf(year_end_list)
  out[[i]]$irr =max(y$irr[y$irr!=Inf] )
  out[[i]]$NPV =sum(year_end_discounted_list)
} 

my_df_new <- unsplit(out, f = my_df$combined_id)
#my_df_new <- unsplit(out, f = my_df$paste.project_id..senario_id.)
my_df_new$PI = my_df_new$NPV/Mod(my_df_new$Total_Capex)
write.csv(my_df_new, "my_df_new_MyData_loc1.csv")

if (levels(my_df_new$Priority) =="1") { 

#my_df1a = sqldf("select project_id, senario_id, sum(year_end) as year_end ,  sum(irr) as irr, sum(NPV) as NPV, combined_id
#               from my_df_new group by project_id ,senario_id having max(NPV) ")

my_df1a = sqldf(paste("select project_id, senario_id, year_id, sum(year_end) as year_end ,  sum(irr) as irr, sum(NPV) as NPV, combined_id
            from my_df_new where year_id <="
            ,time_horizon[1], 
            "group by project_id ,senario_id, year_id having max(NPV)", sep=" "))

my_df1b <- transform(my_df1a, irr = ifelse(irr == -Inf, 0, irr))
my_df1b <- transform(my_df1b, NPV = ifelse(NPV == -Inf, 0, NPV))

write.csv(my_df1b, "my_df1b.csv") 
write.csv(my_df_new, "my_df_new.csv") 

num_of_variables = 20 
my_df_overall = sqldf("select project_id , senario_id  from my_df group by project_id , senario_id  ")
num_of_variables=nrow(my_df_overall)


lprec <- make.lp(0, num_of_variables)
lp.control(lprec,sense="max")
set.type(lprec, 1:num_of_variables, type = "binary")

#set.objfn(lprec, my_df1b$irr[1:num_of_variables])
NPV_df = sqldf("select NPV, combined_id from my_df1b group by combined_id")
set.objfn(lprec, NPV_df$NPV[1:num_of_variables])

#add.constraint(lprec, my_df1b$year_end[1:num_of_variables] , ">=", capex_limit)
RowNames_1 = NULL
for( i in 2016:time_horizon[1])
{
  #print(i)
  RowNames_1 = c(RowNames_1 , paste("C_",i,  sep = ""))
  my_df1b_temp <- my_df1b[my_df1b$year_id==i,]
  add.constraint(lprec, my_df1b_temp$year_end[1:num_of_variables] , ">=", -1*capex_limit)
  rm(my_df1b_temp)
}
#add.constraint(lprec, c(1,1,1,0,0,0,0,0,0), "<=", 1)
#add.constraint(lprec, c(0,0,0,1,1,1,0,0,0), "<=", 1)
#add.constraint(lprec, c(0,0,0,0,0,0,1,1,1), "<=", 1)


## Automatic Equation Generating Code
x=NULL
number_of_project = 5
number_of_scenarios = 4
n <- number_of_project*number_of_scenarios
a <- rep(alist(,)[1], n)
a[1:n]=0
b=unlist(a)

#
for (i in 1:number_of_project) x=rbind(x, b);

#
j=1
for (i in 1:number_of_project) {
  c=x[i,]
  #print(j)
  print(c[j:(j+number_of_scenarios-1)])
  c[j:(j+number_of_scenarios-1)]=1
  print(c[j:(j+number_of_scenarios-1)])
  x[i,]=c
  j=j+number_of_scenarios
  print(x[i,])
  print(j)
}

for (k in 1:nrow(x)){
add.constraint(lprec, x[k,], "<=", 1)
}

# m1 = c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# m2 = c(0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# m3 = c(0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# m4 = c(0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# m5 = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
# m6 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0)
# m7 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0)
# m8 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0)
# m9 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1)
#
# add.constraint(lprec, m1, "<=", 1)
# add.constraint(lprec, m2, "<=", 1)
# add.constraint(lprec, m3, "<=", 1)
# add.constraint(lprec, m4, "<=", 1)
# add.constraint(lprec, m5, "<=", 1)
# add.constraint(lprec, m6, "<=", 1)
# add.constraint(lprec, m7, "<=", 1)
# add.constraint(lprec, m8, "<=", 1)
# add.constraint(lprec, m9, "<=", 1)
#

ColNames <- c("P1", "P2", "P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15", "P16", "P17","P18","P19","P20")
	#,"P21","P22","P23","P24","P25","P26","P27"
RowNames <- c(RowNames_1 , "C1", "C2", "C3", "C4", "C5")
	#, "C6", "C7", "C8", "C9"
dimnames(lprec) <- list(RowNames, ColNames)
lprec
write.lp(lprec, "lpfilename.lp", "lp") #write it to a file in LP format

solve(lprec)
get.objective(lprec)
get.primal.solution(lprec)
get.variables(lprec)

combined_id_unique = unique(my_df1b$combined_id)
Selected_id = data.frame(combined_id_unique[get.variables(lprec)==1])
write.csv(Selected_id , "Selected_id.csv")

rm(lprec)

my_df_1 = sqldf("select * from my_df_new 
                    where combined_id in Selected_id
                    group by project_id ,year_id
                 having max(NPV)      
                order by NPV desc, project_id ,year_id
                ")
			
#
#my_df_2 = my_df_1[order(my_df_1$NPV , decreasing = TRUE),][1:250,]
my_df_2 <- my_df_1
#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)),]

} else if  (levels(my_df_new$Priority) =="2") { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
                 having max( irr) order by irr desc, project_id ,year_id
       ")
#my_df_2 =my_df_1[order(my_df_1$irr, decreasing = TRUE),][1:250,]
my_df_2 <- my_df_1
#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ] 
} else if  (levels(my_df_new$Priority) =="3") { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
                   having max(Revenue) order by Revenue desc, project_id ,year_id
       ")
#my_df_2 =my_df_1[order(my_df_1$Revenue, decreasing = TRUE),][1:250,]
my_df_2 <- my_df_1
#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ]  
} else if  (levels(my_df_new$Priority) =="5") { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
                 having max(PI) order by Revenue desc, project_id ,year_id
       ")
#my_df_2 =my_df_1[order(my_df_1$Revenue, decreasing = TRUE),][1:250,]
my_df_2 <- my_df_1[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ]  
} else if  (levels(my_df_new$Priority) =="4") { 
my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
				having min(Risk) order by Revenue desc, project_id ,year_id
       ")
#my_df_2 =my_df_1[order(my_df_1$Revenue, decreasing = TRUE),][1:250,]
my_df_2 <- my_df_1
#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ]  
} else { 
my_df_1 = sqldf(" select * from my_df_new group by project_id ,year_id
                   having max(NPV)       ")
}

#write.csv(my_df_1, "my_df_1_Prioritized.csv")
write.csv(my_df_2, "my_df_2_Prioritized.csv")

# create a connection # save the password that we can "hide" it as best as we can by collapsing it
pw <- {  "password" }

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# Disconnect a previous connection
dbDisconnect(con) 

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Rio_1",
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