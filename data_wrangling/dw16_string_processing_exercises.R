#16.10 Exercises

#1. Complete all lessons and exercises in the RegexOne7 online interactive tutorial.
#(answer skipped)

#2. In the extdata directory of the dslabs package, you will find a PDF file 
#containing daily mortality data for Puerto Rico from Jan 1, 2015 to May 31, 2018. 
#You can find the file like this:
  
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",
                    package="dslabs")
  
#Find and open the file or open it directly from RStudio.  on Windows, you can type:
  
  system("cmd.exe", input = paste("start", fn))
  
# Which of the following best describes this file:
  
  #It is a report combining graphs and tables. Extracting the data seems possible.