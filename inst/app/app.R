# Deployment entry point for Shiny Server, ShinyProxy or shinyapps.io.
#
# Requires the InterPlotComp package to be installed on the server, together
# with a licensed ASReml-R in the same R installation.
library(InterPlotComp)

shiny::shinyApp(ui = InterPlotComp::app_ui(), server = InterPlotComp::app_server)
