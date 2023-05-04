##-------------------
## Load server´s file
##-------------------
## This script shortcuts to the server´s related files, each representing a part of the application
server <- shinyServer(
   function(input, output, session) {
      for (file in list.files("server")) {
      source(file.path("server", file), local = TRUE, encoding = 'UTF-8')$value
      }
   }
)