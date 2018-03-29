#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ape)
library(ggtree)
library(phangorn)
library(phylocanvas)

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
   
  # Application title
  headerPanel("ggtree Editor"),
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    # Input: Select a file ----
    fileInput("treeFile", "Choose File",
              multiple = FALSE,
              accept = c(".newick",
                         ".nwk"))
  ),
  # Show a plot of the generated distribution
  mainPanel(
    imageOutput("tree"),
    imageOutput("tree_as_svg"),
    phylocanvasOutput("tree_as_phylocanvas")
  )
)

# Define server logic required to draw a tree
server <- function(input, output, session) {
  
  output$tree <- renderPlot({
    req(input$treeFile)
    treeFilePath <- input$treeFile$datapath
    tr <- read.nexus(treeFilePath)
    tree <- ggtree(tr)
    tree
  })
  
  output$tree_as_svg <- renderImage({
    cd <- session$clientData
    width  <- session$clientData$output_tree_as_svg_width
    height <- session$clientData$output_tree_as_svg_height
    mysvgwidth <- width/96
    mysvgheight <- height/96
    
    outfile <- tempfile(fileext='.svg')
    
    req(input$treeFile)
    treeFilePath <- input$treeFile$datapath
    tr <- read.nexus(treeFilePath)
    tree <- ggtree(tr)
    ggsave(file=outfile, plot=tree, width=mysvgwidth, height=mysvgheight)
    
    # Return a list containing the filename
    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = width,
         height = height,
         alt = "tree")
  })
  
  output$tree_as_phylocanvas <- renderPhylocanvas({
    req(input$treeFile)
    treeFilePath <- input$treeFile$datapath
    tr <- read.nexus(treeFilePath)
    tree <- phylocanvas(tr)
    tree
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

