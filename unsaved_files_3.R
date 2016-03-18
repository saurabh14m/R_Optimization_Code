install.packages(XLSX)
install.packages(sqldf)
install.packages("RPostgreSQL")
require(financial);  
require(psych);
require(xlsx); 
# require(financial);  
# require(psych);
require(xlsx); 
require(sqldf)
require(RPostgreSQL)
library(data.table)

project_id <- c(1,	1,	1,	1,	1,	1,	1,	1,	2,	2,	2,	2,	2,	2,	2,	2,	3,	3,	3,	3,	3,	3,	3,	3,	4,	4,	4,	4,	4,	4,	4,	4,	5,	5,	5,	5,	5,	5,	5,	5)

senario_id <- c(1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4,	1,	2,	3,	4)

year_end <- c(2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017,	2016,	2017)

year_id <- c(1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2,	1,	2)

year_end_discounted <- c(3326.04173647534,	154.355830898715,	1780.87412138492,	3614.42743853202,	647.584499847254,	2950.90011231597,	1672.65024189318,	4018.21652138829,	817.478627050757,	1780.22890887658,	3722.90151172786,	1110.58883390649,	2677.58723731684,	2806.14731961967,	4814.6699035407,	3868.07522060844,	2532.45971854868,	3439.25902766985,	2827.04569830687,	2741.02278631596,	3230.25401255576,	1995.00077168519,	85.9120317103601,	3965.51953950695,	1105.78961947198,	1728.4557690858,	4351.28372274149,	600.723872015002,	2471.20014629194,	299.771127723098,	457.798571895316,	2295.13146475752,	2064.16195043294,	3239.08866752549,	4367.20831837952,	27.5038747340778,	3916.36286116488,	1170.60557966668,	2747.88496765143,	4330.16472057614)

Revenue <- c(3326.04173647534,	168.2478556796,	1780.87412138492,	3939.72590799991,	647.584499847254,	3216.4811224244,	1672.65024189318,	4379.85600831323,	817.478627050757,	1940.44951067547,	3722.90151172786,	1210.54182895807,	2677.58723731684,	3058.70057838544,	4814.6699035407,	4216.2019904632,	2532.45971854868,	3748.79234016014,	2827.04569830687,	2987.7148370844,	3230.25401255576,	2174.55084113686,	85.9120317103601,	4322.41629806258,	1105.78961947198,	1884.01678830352,	4351.28372274149,	654.789020496352,	2471.20014629194,	326.750529218177,	457.798571895316,	2501.69329658569,	2064.16195043294,	3530.60664760278,	4367.20831837952,	29.9792234601448,	3916.36286116488,	1275.96008183668,	2747.88496765143,	4719.87954542799)

Risk <- c(0.0075,	0.045,	0.0285,	0.0975,	0.0128,	0.0975,	0.009,	0.054,	0.0675,	0.1125,	0.0285,	0.0975,	0.0285,	0.0825,	0.0285,	0.0675,	0.024,	0.045,	0.054,	0.009,	0.0285,	0.0825,	0.0975,	0.0375,	0.024,	0,	0.135,	0.054,	0,	0.0825,	0.0675,	0.135,	0,	0.033,	0.0105,	0.0975,	0.009,	0.009,	0.0375,	0.018)

Discount_rate <- c(0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09,	0,	0.09)

Priority <- c('1',	'1',	'1',	'1',	'2',	'2',	'2',	'2',	'1',	'1',	'1',	'1',	'2',	'2',	'2',	'2',	'1',	'1',	'1',	'1',	'2',	'2',	'2',	'2',	'1',	'1',	'1',	'1',	'2',	'2',	'2',	'2',	'1',	'1',	'1',	'1',	'2',	'2',	'2',	'2')

Commodity <- c(30,	30,	100,	130,	200,	230,	300,	330,	40,	40,	110,	140,	210,	240,	310,	340,	50,	50,	120,	150,	220,	250,	320,	350,	60,	80,	160,	180,	260,	280,	360,	380,	70,	90,	170,	190,	270,	290,	370,	390)

my_df <- data.frame(project_id ,senario_id , year_end , year_id ,year_end_discounted, paste(project_id,senario_id) , Revenue, Risk , Discount_rate, Priority, Commodity);
my_df <- my_df[my_df$year_end>2015,];
my_df <- my_df[with(my_df, order(project_id, senario_id, year_id)), ]

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

# LOGIC ADDED BY kT FOR DYNAMIC TIME WINDOW
library(data.table)
masterdata <- as.data.table(my_df_new)

x<- c(masterdata[, .(number_of_distinct_scenario = length(unique(senario_id))), ],masterdata[, .(number_of_distinct_project = length(unique(project_id))), ],masterdata[, .(number_of_distinct_year = length(unique(year_id))), ])
prod <- x[[1]]*x[[2]]*x[[3]]
print(prod)

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


library(sqldf)
my_df_1 = sqldf('select * from masterdata group by project_id ,year_id
                having max(NPV)      
                order by NPV desc, project_id ,year_id ')
