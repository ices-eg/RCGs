# ===========================================================================================#
#                                    Server scripts                                          #   
#                                                                                            #
#                                         *                                                  #           
# This script shortcuts to the server related files, contained in the "server" folder,       #
# each representing a part of the application.                                               #
#                                                                                            #
# ===========================================================================================#

##-------------------
## Load server files
##-------------------
server <- shinyServer(function(input, output, session) {
   for (file in list.files("server")) {
      source(file.path("server", file), local = TRUE, encoding = 'UTF-8')$value
   }
})
