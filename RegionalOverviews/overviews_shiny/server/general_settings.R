options(shiny.maxRequestSize = 500*1024^2)

# --------------------------------
# tab selection when click on image
# --------------------------------

shinyjs::onclick("inputID",  updateTabsetPanel(session, inputId="navbar", selected="Input data"))
shinyjs::onclick("inventID",  updateTabsetPanel(session, inputId="navbar", selected="Inventory tables"))
shinyjs::onclick("mapIntID",  updateTabsetPanel(session, inputId="navbar", selected="Interactive map"))
shinyjs::onclick("mapStatID",  updateTabsetPanel(session, inputId="navbar", selected="Static map"))
shinyjs::onclick("plotID",  updateTabsetPanel(session, inputId="navbar", selected="Interactive plots"))
shinyjs::onclick("sampvslandID",  updateTabsetPanel(session, inputId="navbar", selected="Interactive plots"))

# --------------------------------
# modal disclaimer
# --------------------------------

# the modal dialog where the user agree in terms
disclaimer_modal <- modalDialog(
           title = "Terms of use",
           includeHTML("data/DescriptionDisclaimer.txt"),
           easyClose = F,
           footer = tagList( downloadButton("report", "Download Disclaimer"),
                             actionButton("ok", "I agree")
        )     
  )
    
# Show the modal on start up ...
  showModal(disclaimer_modal)
    
# ... close when agree
  observeEvent(input$ok,{
          removeModal()
  })
  
# -------------------------------
# Download disclaimer
# -------------------------------
  
