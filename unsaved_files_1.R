
my_df_new = read.csv("my_df_new.csv")


my_df1a = sqldf(paste("select project_id, senario_id, year_id, sum(year_end) as year_end ,  sum(irr) as irr, sum(NPV) as NPV, combined_id
                      from my_df_new group by project_id ,senario_id, year_id having max(NPV)", sep=" "))

my_df1b <- transform(my_df1a, irr = ifelse(irr == -Inf, 0, irr))
my_df1b <- transform(my_df1b, NPV = ifelse(NPV == -Inf, 0, NPV))

head(my_df1b) 


lprec
library(lpSolveAPI)

lprec = read.lp("lpfilename.lp", "lp") #write it to a file in LP format
my_df1b = read.csv("my_df1b.csv")


solve(lprec)
get.objective(lprec)
get.primal.solution(lprec)
get.variables(lprec)

combined_id_unique = unique(my_df1b$combined_id)
Selected_id = data.frame(combined_id_unique[get.variables(lprec)==1])

write.csv(Selected_id , "Selected_id.csv")
