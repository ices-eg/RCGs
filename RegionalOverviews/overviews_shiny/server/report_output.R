# *********************
# define the report format of the output 
# *********************

output$report <- downloadHandler(
  filename = function() {
    paste("disclaimer_", Sys.Date(), ".pdf", sep='')
  },
  
  content = function(file) {
    file.copy("www/01_hello.pdf", file)
  }
)
# old version for a .docx variante            
#      filename = function() {
#         paste('report_', Sys.Date(), '.docx', sep='')
#      },						
#      
#      content = function(file) {
#         x <- read_docx()
#         print(x, target = file)
#      }
#   )
