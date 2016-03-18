
my_df <- data.frame(project_id ,senario_id , year_end , year_id ,year_end_discounted, paste(project_id,senario_id) , Revenue, Risk , Discount_rate, Priority, Commodity, Total_Capex);
#write.xlsx(my_df, "MyData_In.xlsx")
write.csv(my_df, "my_df_IN.csv");
write.csv(as.data.frame(.arg2), "my_df_IN2.csv");
write.csv(as.data.frame(.arg3), "my_df_IN3.csv");

senario_id_new = senario_id +1


my_df <- subset(my_df , my_df$year_id>2015  & my_df$year_id<=time_horizon);

my_df <- my_df[with(my_df, order(project_id, senario_id, year_id)), ]

write.xlsx(my_df, "MyData_In.xlsx")
names(my_df)[6]="combined_id"

out <- split( my_df , f = my_df$combined_id)
#out <- split( my_df , f = my_df$paste.project_id..senario_id.)

irr <- NULL
 
for( i in 1:length(out))
{
  year_end_list = out[[i]]$year_end
  year_end_discounted_list = out[[i]]$year_end_discounted
  y = cf(year_end_list)
  out[[i]]$irr =max(y$irr)
  out[[i]]$NPV =sum(year_end_discounted_list)
} 

my_df_new <- unsplit(out, f = my_df$combined_id)
#my_df_new <- unsplit(out, f = my_df$paste.project_id..senario_id.)
my_df_new$PI = my_df_new$NPV/Mod(my_df_new$Total_Capex)
write.csv(my_df_new, "my_df_new_MyData_loc1.csv")

