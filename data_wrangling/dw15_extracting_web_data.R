#Data Wrangling: Extracting data from the web 
#Exercises 15.4 

#1. Visit the following web page: https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm

#Notice there are several tables. Say we are interested in comparing the 
#payrolls of teams across the years. The next few exercises take us through 
#the steps needed to do this.

#Start by applying what you learned to read in the website into 
#an object called h_ex1.

url_ex1 <- 
  paste('https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm')
h_ex1 <- read_html(url_ex1)
View(h_ex1)

#2. Note that, although not very useful, we can actually see the content of the page by typing:
#html_text(h_ex1)
html_text(h_ex1)

#The next step is to extract the tables. For this, we can use the html_nodes 
#function. We learned that tables in html are associated with the table node. 
#Use the html_nodes function to extract all tables. Store it in an object tab_ex1.

tab_ex1 <- h_ex1 |> html_nodes('table')

#3. The html_nodes function returns a list of objects of class xml_node. 
#We can see the content of each one using, for example, the html_text function. 
#You can see the content for an arbitrarily picked component like this:

html_text(nodes[[8]])

#If the content of this object is an html table, we can use the html_table 
#function to convert it to a data frame. Use the html_table function to convert 
#the 8th entry of nodes into a table.

html_text(tab_ex1[[8]])
tab_ex3 <- tab_ex1[[8]] |> html_table()
 class(tab_ex3)
#[1] "tbl_df"     "tbl"        "data.frame"
head(tab_ex3) 
 # A tibble: 6 × 4
# X1      X2                    X3           X4        
#<chr>    <chr>                 <chr>        <chr>     
# 1 No.   Team                  Payroll      Average   
# 2 1.    New York Yankees      $228,835,490 $7,151,109
# 3 2.    Los Angeles Dodgers   $216,597,577 $7,468,882
# 4 3.    Philadelphia Phillies $165,385,714 $6,125,397

#6.We have learned that the first and last entries of nodes are not payroll 
#tables. Redefine nodes so that these two are removed.

tab_ex1[[22]] <- NULL
tab_ex1[[1]] <- NULL

#alternative way
tab_ex1 <- tab_ex1[2:(length(tab_ex1) - 1)]

#7.We saw in the previous analysis that the first table node is not actually a 
#table. This happens sometimes in html because tables are used to make text look
#a certain way, as opposed to storing numeric values. Remove the first component
#and then use sapply and html_table to convert each node in nodes into a table. 
#Note that in this case, sapply will return a list of tables. You can also use 
#lapply to assure that a list is applied.

#sapply()
nodes_tables_simp <- nodes |> sapply(html_table)

#lapply()
nodes_tables <- nodes |> lapply(html_table)

#8. Look through the resulting tables. Are they all the same? Could we just 
#join them with bind_rows?
nodes_bind <- bind_rows(nodes_tables, .id = 'id')

#9 Create two tables, call them tab_1 and tab_2 using the 10th and 19th tables
#in nodes.
tab1 <- nodes_tables[[10]]
tab2 <- nodes_tables[[19]]

#10. Use a full_join function to combine these two tables. Before you do this 
#you will have to fix the missing header problem. You will also need to make the
#names match.

#tab1
colnames(tab1)<- c('Team', 'Payroll', 'Average')
tab1 <- tab1[-1, ]

#tab2
colnames(tab2)<- c('Team', 'Payroll', 'Average')
tab1 <- tab1[-1, ]

full_join(tab1, tab2, by='Team')
    

#11. After joining the tables, you see several NAs. This is because some teams 
#are in one table and not the other. Use the anti_join function to get a better
#idea of why this is happening.
anti_join(tab1, tab2, by='Team')
# A tibble: 5 × 3
#X1                  X2           X3        
#<chr>               <chr>        <chr>     
#1 N.Y. Yankees        $201,449,289 $7,748,050
#2 New York Mets       135,773,988  4,849,071 

#12. We see see that one of the problems is that Yankees are listed as both N.Y.
#Yankees and NY Yankees. In the next section, we will learn efficient approaches
#to fixing problems like this.

#Here we can do it “by hand” as follows:
  
  tab_1 <- tab_1 |>
  mutate(Team = ifelse(Team == "N.Y. Yankees", "NY Yankees", Team))

#Now join the tables and show only Oakland and the Yankees and the payroll columns.

#no returned data  
joined_tables <- full_join(tab1, tab2, by='Team') |> filter(Team =='Oakland' & Team == 'NY Yankees')  

#Yankees  
joined_tables <- full_join(tab1, tab2, by='Team') |> filter(Team =='NY Yankees')

#Oakland
joined_tables <- full_join(tab1, tab2, by='Team') |> filter(Team =='Oakland')

