
my_df <- data.frame(project_id ,senario_id , year_end , year_id ,year_end_discounted, paste(project_id,senario_id) , Revenue, Risk , Discount_rate, Priority, Commodity, Total_Capex);
write.csv(my_df, "MyData_In.csv")
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
  out[[i]]$added_capex =sum(Total_Capex)

  } 	

my_df_new <- unsplit(out, f = my_df$combined_id)
#my_df_new <- unsplit(out, f = my_df$paste.project_id..senario_id.)
my_df_new$PI = Mod(my_df_new$NPV)/Mod(my_df_new$added_capex)
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

#write.csv(my_df1b, "my_df1b.csv") 
#write.csv(my_df_new, "my_df_new.csv") 

## Automatation Code 01
my_df_overall = sqldf("select project_id, senario_id  from my_df group by project_id ,senario_id  ")
my_df_project_id = sqldf("select distinct( project_id) from my_df")

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


## Automatic Equation Generating Code 02

x=NULL
number_of_project = nrow(my_df_project_id)
number_of_scenarios = 4
n <- nrow(my_df_overall)
a <- rep(alist(,)[1], n)
a[1:n]=0
b=unlist(a)

#
for (i in 1:number_of_project) x=rbind(x, b);

#
j=1
for (i in 1:number_of_project) {
  c=x[i,]
  number_of_scenarios = unlist(sqldf(paste("select count(distinct(senario_id)) from my_df where project_id=",my_df_project_id[i,], sep=)))
  
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

ColNames <- c("P1", "P2", "P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15", "P16"
				,"P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37","P38","P39","P40","P41","P42","P43")
RowNames_0 <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15")				
RowNames <- c(RowNames_1 , RowNames_0[1:number_of_project])
dimnames(lprec) <- list(RowNames, ColNames[1:num_of_variables])
lprec
write.lp(lprec, "lpfilename.lp", "lp") #write it to a file in LP format

solve(lprec)
get.objective(lprec)
get.primal.solution(lprec)
get.variables(lprec)

combined_id_unique = unique(my_df1b$combined_id)
Selected_id = data.frame(combined_id_unique[get.variables(lprec)==1])
#write.csv(Selected_id , "Selected_id.csv")

rm(lprec)

my_df_1 = sqldf("select * from my_df_new 
                    where combined_id in Selected_id
                    group by project_id ,senario_id, year_id
                 having max(NPV)      
                order by NPV desc, project_id ,year_id
                ")
			
#
#my_df_2 = my_df_1[order(my_df_1$NPV , decreasing = TRUE),][1:250,]
my_df_2 <- sqldf("select my_df_new.project_id , my_df_new.senario_id , my_df_new.year_id , my_df_1.year_end_discounted,my_df_1.combined_id,my_df_1.Revenue,my_df_1.Risk, my_df_1.Discount_rate,my_df_1.Priority,my_df_1.Commodity,my_df_1.Total_Capex,my_df_1.irr,my_df_1.NPV,my_df_1.PI  
from my_df_new  left outer join my_df_1 USING(project_id, senario_id, 
			 year_id)       			")

##"my_df_1.year_end_discounted,my_df_1.combined_id,my_df_1.Revenue,my_df_1.Risk",my_df_1.Discount_rate,my_df_1.Priority,my_df_1.Commodity,my_df_1.Total_Capex,my_df_1.irr,my_df_1.NPV,my_df_1.PI
			 
			 
			 
#my_df_2x = merge(x=my_df_new, y=my_df_1, by = c("project_id","senario_id","year_id") , all.x = TRUE)	   
write.csv(my_df_2	, "my_df_2x.csv")
#my_df_2 = my_df_1	   

#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)),]

} else if  (levels(my_df_new$Priority) =="2") { 
#my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id
#                 having max( irr) order by irr desc, project_id ,year_id
#       ")

my_df1a = sqldf(paste("select project_id, senario_id, year_id, sum(year_end) as year_end ,  sum(irr) as irr, sum(NPV) as NPV, combined_id
            from my_df_new where year_id <="
            ,time_horizon[1], 
            "group by project_id ,senario_id, year_id having max(irr)", sep=" "))

my_df1b <- transform(my_df1a, irr = ifelse(irr == -Inf, 0, irr))
my_df1b <- transform(my_df1b, NPV = ifelse(NPV == -Inf, 0, NPV))

#write.csv(my_df1b, "my_df1b.csv") 
#write.csv(my_df_new, "my_df_new.csv") 

## Automatation Code 01
my_df_overall = sqldf("select project_id, senario_id  from my_df group by project_id ,senario_id  ")
my_df_project_id = sqldf("select distinct( project_id) from my_df")

num_of_variables=nrow(my_df_overall)

lprec <- make.lp(0, num_of_variables)
lp.control(lprec,sense="max")
set.type(lprec, 1:num_of_variables, type = "binary")

#set.objfn(lprec, my_df1b$irr[1:num_of_variables])
irr_df = sqldf("select irr, combined_id from my_df1b group by combined_id")
set.objfn(lprec, irr_df$irr[1:num_of_variables])

RowNames_1 = NULL
for( i in 2016:time_horizon[1])
{
  #print(i)
  RowNames_1 = c(RowNames_1 , paste("C_",i,  sep = ""))
  my_df1b_temp <- my_df1b[my_df1b$year_id==i,]
  add.constraint(lprec, my_df1b_temp$year_end[1:num_of_variables] , ">=", -1*capex_limit)
  rm(my_df1b_temp)
}


## Automatic Equation Generating Code 02

x=NULL
number_of_project = nrow(my_df_project_id)
number_of_scenarios = 4
n <- nrow(my_df_overall)
a <- rep(alist(,)[1], n)
a[1:n]=0
b=unlist(a)

#
for (i in 1:number_of_project) x=rbind(x, b);

#
j=1
for (i in 1:number_of_project) {
  c=x[i,]
  number_of_scenarios = unlist(sqldf(paste("select count(distinct(senario_id)) from my_df where project_id=",my_df_project_id[i,], sep=)))
  
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

ColNames <- c("P1", "P2", "P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15", "P16"
				,"P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37","P38","P39","P40","P41","P42","P43")
RowNames_0 <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15")				
RowNames <- c(RowNames_1 , RowNames_0[1:number_of_project])
dimnames(lprec) <- list(RowNames, ColNames[1:num_of_variables])
lprec
write.lp(lprec, "lpfilename_irr.lp", "lp") #write it to a file in LP format

solve(lprec)
get.objective(lprec)
get.primal.solution(lprec)
get.variables(lprec)

combined_id_unique = unique(my_df1b$combined_id)
Selected_id = data.frame(combined_id_unique[get.variables(lprec)==1])
#write.csv(Selected_id , "Selected_id.csv")

rm(lprec)

my_df_1 = sqldf("select * from my_df_new 
                    where combined_id in Selected_id
                    group by project_id ,senario_id, year_id
                 having max(irr)      
                order by irr desc, project_id ,year_id
                ")
			
#

my_df_2 <- sqldf("select my_df_new.project_id , my_df_new.senario_id , my_df_new.year_id , my_df_1.year_end_discounted,my_df_1.combined_id,my_df_1.Revenue,my_df_1.Risk, my_df_1.Discount_rate,my_df_1.Priority,my_df_1.Commodity,my_df_1.Total_Capex,my_df_1.irr,my_df_1.NPV,my_df_1.PI  
from my_df_new  left outer join my_df_1 USING(project_id, senario_id, 
			 year_id)       			")

write.csv(my_df_2	, "my_df_2xb.csv")
	   
	   
	   
	   
	   
	   
#my_df_2 =my_df_1[order(my_df_1$irr, decreasing = TRUE),][1:250,]
#my_df_2 <- my_df_1
#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ] 

} else if  (levels(my_df_new$Priority) =="3") { 
#my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id having max(Revenue) order by Revenue desc, project_id ,year_id")
	   
  
my_df1a = sqldf(paste("select project_id, senario_id, year_id, sum(year_end) as year_end ,  sum(irr) as irr, sum(NPV) as NPV, sum(Revenue) as Revenue ,combined_id
            from my_df_new where year_id <="
            ,time_horizon[1], 
            "group by project_id ,senario_id, year_id having max(Revenue)", sep=" "))

my_df1b <- transform(my_df1a, Revenue = ifelse(Revenue < 0, 0, Revenue))
my_df1b <- transform(my_df1b, NPV = ifelse(NPV == -Inf, 0, NPV))

#write.csv(my_df1b, "my_df1b.csv") 
#write.csv(my_df_new, "my_df_new.csv") 

## Automatation Code 01
my_df_overall = sqldf("select project_id, senario_id  from my_df group by project_id ,senario_id  ")
my_df_project_id = sqldf("select distinct( project_id) from my_df")

num_of_variables=nrow(my_df_overall)

lprec <- make.lp(0, num_of_variables)
lp.control(lprec,sense="max")
set.type(lprec, 1:num_of_variables, type = "binary")

#set.objfn(lprec, my_df1b$irr[1:num_of_variables])
Revenue_df = sqldf("select Revenue, combined_id from my_df1b group by combined_id")
set.objfn(lprec, Revenue_df$Revenue[1:num_of_variables])

RowNames_1 = NULL
for( i in 2016:time_horizon[1])
{
  #print(i)
  RowNames_1 = c(RowNames_1 , paste("C_",i,  sep = ""))
  my_df1b_temp <- my_df1b[my_df1b$year_id==i,]
  add.constraint(lprec, my_df1b_temp$year_end[1:num_of_variables] , ">=", -1*capex_limit)
  rm(my_df1b_temp)
}


## Automatic Equation Generating Code 02

x=NULL
number_of_project = nrow(my_df_project_id)
number_of_scenarios = 4
n <- nrow(my_df_overall)
a <- rep(alist(,)[1], n)
a[1:n]=0
b=unlist(a)

#
for (i in 1:number_of_project) x=rbind(x, b);

#
j=1
for (i in 1:number_of_project) {
  c=x[i,]
  number_of_scenarios = unlist(sqldf(paste("select count(distinct(senario_id)) from my_df where project_id=",my_df_project_id[i,], sep=)))
  
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

ColNames <- c("P1", "P2", "P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15", "P16"
				,"P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37","P38","P39","P40","P41","P42","P43")
RowNames_0 <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15")				
RowNames <- c(RowNames_1 , RowNames_0[1:number_of_project])
dimnames(lprec) <- list(RowNames, ColNames[1:num_of_variables])
lprec
write.lp(lprec, "lpfilename_Revenue.lp", "lp") #write it to a file in LP format

solve(lprec)
get.objective(lprec)
get.primal.solution(lprec)
get.variables(lprec)

combined_id_unique = unique(my_df1b$combined_id)
Selected_id = data.frame(combined_id_unique[get.variables(lprec)==1])
#write.csv(Selected_id , "Selected_id.csv")

rm(lprec)

my_df_1 = sqldf("select * from my_df_new 
                    where combined_id in Selected_id
                    group by project_id ,senario_id, year_id
                 having max(Revenue)      
                order by Revenue desc, project_id ,year_id
                ")
			
#

my_df_2 <- sqldf("select my_df_new.project_id , my_df_new.senario_id , my_df_new.year_id , my_df_1.year_end_discounted,my_df_1.combined_id,my_df_1.Revenue,my_df_1.Risk, my_df_1.Discount_rate,my_df_1.Priority,my_df_1.Commodity,my_df_1.Total_Capex,my_df_1.irr,my_df_1.NPV,my_df_1.PI  
from my_df_new  left outer join my_df_1 USING(project_id, senario_id, 
			 year_id)       			")

write.csv(my_df_2	, "my_df_2xc.csv")
	   
	   

	   
#my_df_2 =my_df_1[order(my_df_1$Revenue, decreasing = TRUE),][1:250,]
#my_df_2 <- my_df_1
#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ]  
} else if  (levels(my_df_new$Priority) =="5") { 


my_df1a = sqldf(paste("select project_id, senario_id, year_id, sum(year_end) as year_end ,  sum(irr) as irr, sum(NPV) as NPV, sum(PI) as PI ,combined_id
            from my_df_new where year_id <="
            ,time_horizon[1], 
            "group by project_id ,senario_id, year_id having max(PI)", sep=" "))

my_df1b <- transform(my_df1a, PI = ifelse(PI < 0, 0, PI))
my_df1b <- transform(my_df1b, NPV = ifelse(NPV == -Inf, 0, NPV))

#write.csv(my_df1b, "my_df1b.csv") 
#write.csv(my_df_new, "my_df_new.csv") 

## Automatation Code 01
my_df_overall = sqldf("select project_id, senario_id  from my_df group by project_id ,senario_id  ")
my_df_project_id = sqldf("select distinct( project_id) from my_df")

num_of_variables=nrow(my_df_overall)

lprec <- make.lp(0, num_of_variables)
lp.control(lprec,sense="max")
set.type(lprec, 1:num_of_variables, type = "binary")

#set.objfn(lprec, my_df1b$irr[1:num_of_variables])
PI_df = sqldf("select PI, combined_id from my_df1b group by combined_id")
set.objfn(lprec, PI_df$PI[1:num_of_variables])

RowNames_1 = NULL
for( i in 2016:time_horizon[1])
{
  #print(i)
  RowNames_1 = c(RowNames_1 , paste("C_",i,  sep = ""))
  my_df1b_temp <- my_df1b[my_df1b$year_id==i,]
  add.constraint(lprec, my_df1b_temp$year_end[1:num_of_variables] , ">=", -1*capex_limit)
  rm(my_df1b_temp)
}


## Automatic Equation Generating Code 02

x=NULL
number_of_project = nrow(my_df_project_id)
number_of_scenarios = 4
n <- nrow(my_df_overall)
a <- rep(alist(,)[1], n)
a[1:n]=0
b=unlist(a)

#
for (i in 1:number_of_project) x=rbind(x, b);

#
j=1
for (i in 1:number_of_project) {
  c=x[i,]
  number_of_scenarios = unlist(sqldf(paste("select count(distinct(senario_id)) from my_df where project_id=",my_df_project_id[i,], sep=)))
  
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

ColNames <- c("P1", "P2", "P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15", "P16"
				,"P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37","P38","P39","P40","P41","P42","P43")
RowNames_0 <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15")				
RowNames <- c(RowNames_1 , RowNames_0[1:number_of_project])
dimnames(lprec) <- list(RowNames, ColNames[1:num_of_variables])
lprec
write.lp(lprec, "lpfilename_PI.lp", "lp") #write it to a file in LP format

solve(lprec)
get.objective(lprec)
get.primal.solution(lprec)
get.variables(lprec)

combined_id_unique = unique(my_df1b$combined_id)
Selected_id = data.frame(combined_id_unique[get.variables(lprec)==1])
#write.csv(Selected_id , "Selected_id.csv")

rm(lprec)

my_df_1 = sqldf("select * from my_df_new 
                    where combined_id in Selected_id
                    group by project_id ,senario_id, year_id
                 having max(PI)      
                order by PI desc, project_id ,year_id
                ")
			
#

my_df_2 <- sqldf("select my_df_new.project_id , my_df_new.senario_id , my_df_new.year_id , my_df_1.year_end_discounted,my_df_1.combined_id,my_df_1.Revenue,my_df_1.Risk, my_df_1.Discount_rate,my_df_1.Priority,my_df_1.Commodity,my_df_1.Total_Capex,my_df_1.irr,my_df_1.NPV,my_df_1.PI  
from my_df_new  left outer join my_df_1 USING(project_id, senario_id, 
			 year_id)       			")

write.csv(my_df_2	, "my_df_2xd.csv")


#my_df_2 =my_df_1[order(my_df_1$Revenue, decreasing = TRUE),][1:250,]
#my_df_2 <- my_df_1[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ]  

} else if  (levels(my_df_new$Priority) =="4") { 
#my_df_1 = sqldf("select * from my_df_new group by project_id ,year_id		having min(Risk) order by Revenue desc, project_id ,year_id")

my_df1a = sqldf(paste("select project_id, senario_id, year_id, sum(year_end) as year_end ,  sum(irr) as irr, sum(Risk) as Risk, combined_id
            from my_df_new where year_id <="
            ,time_horizon[1], 
            "group by project_id ,senario_id, year_id ", sep=" "))

#my_df1b <- transform(my_df1a, irr = ifelse(irr == -Inf, 0, irr))
my_df1b <- transform(my_df1a, Risk = ifelse(Risk <=0, 0.0001, Risk))

write.csv(my_df1a, "my_df1a.csv") 
#write.csv(my_df_new, "my_df_new.csv") 

## Automatation Code 01
my_df_overall = sqldf("select project_id, senario_id  from my_df group by project_id ,senario_id  ")
my_df_project_id = sqldf("select distinct( project_id) from my_df")

num_of_variables=nrow(my_df_overall)

lprec <- make.lp(0, num_of_variables)
lp.control(lprec,sense="min")
set.type(lprec, 1:num_of_variables, type = "binary")

#set.objfn(lprec, my_df1b$irr[1:num_of_variables])
Risk_df = sqldf("select Risk, combined_id from my_df1b group by combined_id")
set.objfn(lprec, Risk_df$Risk[1:num_of_variables])

RowNames_1 = NULL
for( i in 2016:time_horizon[1])
{
  #print(i)
  RowNames_1 = c(RowNames_1 , paste("C_",i,  sep = ""))
  my_df1b_temp <- my_df1b[my_df1b$year_id==i,]
  add.constraint(lprec, my_df1b_temp$year_end[1:num_of_variables] , ">=", -1*capex_limit)
  rm(my_df1b_temp)
}


## Automatic Equation Generating Code 02

x=NULL
number_of_project = nrow(my_df_project_id)
number_of_scenarios = 4
n <- nrow(my_df_overall)
a <- rep(alist(,)[1], n)
a[1:n]=0
b=unlist(a)

#
for (i in 1:number_of_project) x=rbind(x, b);

#
j=1
for (i in 1:number_of_project) {
  c=x[i,]
  number_of_scenarios = unlist(sqldf(paste("select count(distinct(senario_id)) from my_df where project_id=",my_df_project_id[i,], sep=)))
  
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

ColNames <- c("P1", "P2", "P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15", "P16"
				,"P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37","P38","P39","P40","P41","P42","P43")
RowNames_0 <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15")				
RowNames <- c(RowNames_1 , RowNames_0[1:number_of_project])
dimnames(lprec) <- list(RowNames, ColNames[1:num_of_variables])
lprec
write.lp(lprec, "lpfilename_Risk.lp", "lp") #write it to a file in LP format

solve(lprec)
get.objective(lprec)
get.primal.solution(lprec)
get.variables(lprec)

combined_id_unique = unique(my_df1b$combined_id)
Selected_id = data.frame(combined_id_unique[get.variables(lprec)==1])
#write.csv(Selected_id , "Selected_id.csv")

rm(lprec)

my_df_1 = sqldf("select * from my_df_new 
                    where combined_id in Selected_id
                    group by project_id ,senario_id, year_id
                 having max(irr)      
                order by irr desc, project_id ,year_id
                ")
			
#

my_df_2 <- sqldf("select my_df_new.project_id , my_df_new.senario_id , my_df_new.year_id , my_df_1.year_end_discounted,my_df_1.combined_id,my_df_1.Revenue,my_df_1.Risk, my_df_1.Discount_rate,my_df_1.Priority,my_df_1.Commodity,my_df_1.Total_Capex,my_df_1.irr,my_df_1.NPV,my_df_1.PI  
from my_df_new  left outer join my_df_1 USING(project_id, senario_id, year_id) 	")

write.csv(my_df_2	, "my_df_2xe.csv")
	   				
				
				
#my_df_2 =my_df_1[order(my_df_1$Revenue, decreasing = TRUE),][1:250,]
#my_df_2 <- my_df_1
#[my_df_1$year_id<=as.numeric(gsub(",","",time_horizon)), ]  
} else { 
my_df_2 = sqldf(" select * from my_df_new group by project_id ,year_id
                   having max(NPV)       ")
}

#write.csv(my_df_1, "my_df_1_Prioritized.csv")
#write.csv(my_df_2, "my_df_2_Prioritized.csv")

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
#df_Commodities <- dbGetQuery(con, "SELECT * from \"Commodities\" ")

#dbGetQuery(con, "drop table  \"Prioritized\" ")

#dbWriteTable(con, "Prioritized", my_df_2, row.names=FALSE)
dbDisconnect(con)


