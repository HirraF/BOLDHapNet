# Shiny App GUI TRIAL 1. 
library(shiny)
source("Functions-Fin.R")

#Columns to choose from for the the pie chart function. 
piechoices<-c("country","province_state","region","species_name","bin_uri",
              "processid","sex", "institution_storing","identification_provided_by",
              "lat","lon","lifestage","reproduction","phylum_name","class_name",
              "order_name","family_name","subfamily_name","subspecies_name",
              "sampleid", "recordID", "catalognum", "fieldnum","collection_code",
              "identification_method","identification_reference","tax_note", 
              "voucher_status","tissue_type", "collectors","collection_event_id",
              "collectiondate_start","collectiondate_end","collectiontime",
              "collection_note", "site_code","sampling_protocol","habitat", 
              "associated_specimens","associated_taxa","coord_source" ,"coord_accuracy",
              "elev","elev_accuracy","depth","depth_accuracy" )

colorders <- c("processid", "bin_uri", "species_name", 
             "subspecies_name", "genus_name", "family_name",
             "order_name", "class_name", "phylum_name", "country",
             "province_state", "region", "institution_storing",
             "identification_provided_by")

table_options <- list(
    scrollX = TRUE,
    scrollY = 500,
    fixedColumns = list(leftColumns = 4),
    dom = 'Bfrtip',
    selection = 'none',
    buttons = list(list(extend = 'csv', text =
                            "Save as CSV")))

# UI ---------------------------------------------------------------------------
ui <- fluidPage(

# APP HEADER -------------------------------------------------------------------
    tags$div( 
        h1("BOLDHapNet: Haplotype Network Creator"),
        style = ("top:0;left:0;height:100px; position:absolute; width:100%; 
                 background:#235952 ; color: white; z-index: 3; margin:0; 
                 text-align:center"
                 ) 
        ),
# CSS STYLE TAGS ---------------------------------------------------------------
    tags$head(
        tags$style(HTML("body, pre{ height: 100%}")), # HTML Page body
        tags$style(HTML("h5 { background:#fb5b5a }")), #Styling for Step titles
        tags$style(HTML("#tabletitle {background:#fb5b5a; font-size:15px; 
                        font-weight:bold; text-align:center}" )), #Styling for table titles 
        tags$style(HTML(".shiny-notification {position:fixed;top: 
                        calc(50%);left: calc(50%);}")), #styling for Notifications (error pop-up)
        tags$style(HTML("#leftpaneltext {font-size:12px}")),
        tags$style(HTML(".myclass {background-color: #EFF8CD; font-size:10px}")),
        tags$style(HTML("#leftpaneldiv {width: 100%; margin:auto; text-align: center;}")),
        tags$style(HTML(".superbigimage{ overflow-x:scroll; white-space: nowrap; }")),
        tags$style(HTML(".superbigimage img{ max-width: none; }")),
        
            
        ),
# SIDE PANEL: INPUTS  ----------------------------------------------------------
fluidRow(
    column(
        width=3, id = 'column1',
        style = ("background:#c9d1d3; height:100%;top:100px;left:0px;
                 position:absolute; overflow-y:scroll"), 
        #£ INFORMATION AND INSTRUCTION PANEL ----
            div(
                style = ("width:100%; overflow:scroll; height: 300px; background:#e3e7e8"),
                h4("Instructions for using this tool: "),
                tags$ol(
                    tags$li("Load data from", 
                            tags$a(href="http://www.boldsystems.org","BOLD Systems."
                                   )),
                    tags$li("Select which barcoding gene to use "),
                    tags$li("Decide whether to run a 'Greedy Search'. 
                    Greedy Search retrieves all records from all the bins in 
                            your current dataset"),
                    tags$li("Filter the table to retain your required records"),
                    tags$li("Sequences for the records will be prepared. 
                    You can remove any sequences of low quality. 
                    "),
                    tags$li("You can trim the alignment to only analyse a certain 
                            segment of them if you wish."),
                    tags$li("Select a variable to use to create haplotype pie charts."),
                    tags$li("Choose an edge style for your graph."),
                    tags$li("GENERATE YOUR NETWORK!")
                ),
                "As each step appears you can click the ? icon to get more info on the step. "
            ),
        ## £INPUT: FOR BOLD SYSTEM SEARCH STRING.------
        div(
            id="leftpaneldiv",
            h5("Step 1. BOLD Search", actionLink('help1','?')),
            # user type in search string 
            textInput("boldsearchstr", "Type in your BOLD search"),
            # submit the search
            actionButton('boldsearchbtn', 'SEARCH BOLD'),
            br()
            ), 
        # INPUT: SELECT WHICH GENE MARKER
        div(
            id="leftpaneldiv",
            uiOutput("geneselector") # generated in server following BOLD download
        ),
        # INPUT: SELECT GREEDY SEARCH 
        conditionalPanel(
            condition = "input.selectagene > 0", # to hide
            div(
                id="leftpaneldiv",
                h5("Step 3. Greedy search option.", actionLink("help3","?")),
                radioButtons("greedycheckbox", 
                             "Greedy Search Required? ",
                             choices = c("Yes", "No"),
                             selected = "Yes", inline = TRUE),
                actionButton("continuegreedy", "CONTINUE")
            )
            ), 
        # INPUT: Select which Filter Tool to use. 
        conditionalPanel(
            condition = "input.continuegreedy > 0",
            div(id="leftpaneldiv",
                h5("Step 4. Filter table.",actionLink("help4","?")),
                div(id='leftpaneldiv', style=("text-align: left;"),
                    p(id='leftpaneltext', 
                    "Here you can remove any unwanted rows from your table.", br(), 
                    "To remove rows matching a specific condition, try the interactive", strong("Simple "), "tool.",
                    br(),
                    "For more complex queries, try the", strong("Advanced "), "tool.
                    Here you can type conditional expressions (evaluating to TRUE or FALSE). 
                    Rows where the expression evaluates to true will be removed.",
                    br(),
                    "Click the ? for more information on the Simple and Advanced Tools.",
                    br(),
                    "Select ", strong("None "), "if you don't want to remove any records. ")
                ),
                radioButtons( "filtertype",
                "Select which tool you want to use to help remove unwanted rows.",
                    choices = c("None", "Simple", "Advanced"),
                    selected = "None",
                    inline = TRUE
                )
                
            ),
            conditionalPanel(
                condition = "input.filtertype == 'Simple'",
                div(id="leftpaneldiv",
                  div(h6("Simple Filter Tool"), style="background:#fb5b5a ")  
                  ,
                  p(id='leftpaneltext', strong("Click 'Select Filter' button on current table to 
                  select filters. Rows matching the selected filters will be 
                  removed. Click 'See Table' to inspect your final dataset with 
                    rows removed")
                  )
                )
            ),
            # user chooses to use text based filtering
            conditionalPanel(
                condition = "input.filtertype == 'Advanced'",
                div(id="leftpaneldiv",
                    div(h6("Advanced Filter Tool ",actionLink("help4a","?")), style="background:#fb5b5a "),
                    p(id = 'leftpaneltext',
                        strong("Type a filter statement below. The statement should
                       select rows you want to remove. Click FILTER to see which rows
                       you have selected for removal. When you are satified with
                       the rows you selected for removal click 'See Table' to inspect
                        your final dataset"),
                       br()
                    ),
                    # user input their statement 
                    textAreaInput('filterstatement', "Type table filter statement below: "),
                    actionButton('filterbtn', "FILTER"), # submit 
                    br(), br()
                )
            ),
            # user chooses no filtering of table
            conditionalPanel(
                condition = "input.filtertype == 'None'",
                div(id="leftpaneldiv",
                    div(h6("No filter"), style="background:#fb5b5a "),
                p(id="leftpaneltext",strong("No rows will be removed."))
                )
            ),   
            # Once user satisifed with filter applied, allow final submission of table 
            div(id="leftpaneldiv",
                p(id="leftpaneltext",
                  "Click 'See Table' to inspect your final dataset"),
                # allow user to see what table is like with their rows removed.
                actionButton("check1", "See Table"),
                p(id="leftpaneltext",
                    "Once you are satisfied with your final dataset Click CONTINUE",
                div(style="text-align: center;",
                    actionButton('finalconfirm', "CONTINUE")), 
                br())
            )
        ),
       
        # SELECT TO REMOVE ANY SEQS
        conditionalPanel(
                condition = "input.finalconfirm > 0",
                div(style="text-align: center;",
                    radioButtons("selectgraphics","Show Graphical view of MSA?",
                                 choices = c("No","Yes"),
                                 selected = "No",
                                 inline = TRUE)),
                
                div(id="leftpaneldiv",
                    uiOutput("sequenceremove"))
            ),
            # TRIM SEQUENCES
            conditionalPanel(
                condition = "input.selecturseqs > 0",
                div(id="leftpaneldiv", br(),
                    h5("Step 6. Option to trim sequences.", actionLink("help6", "?")),
                    radioButtons('trimseqs','Would you like to trim sequences? ',
                                 choices = c("Yes", "No"),
                                 selected = "No",inline = TRUE)
                )
            ),
        # IF USER SELECTS TO TRIM: Present following panel. 
        conditionalPanel(
            condition = "input.trimseqs == 'Yes' && input.selecturseqs >0",
            div(id="leftpaneldiv",
                br(), #define seq start and end
                numericInput( "trimstart", "Sequence Start: ", value = 1,min = 1),
                numericInput("trimend", "Sequence End: ", value = 1, min = 1),
                actionButton("trimtheseq", "TRIM"), #Submit 
                br()
                )
            ),  
        # If User does not want to trim seqs: Present Continue button.
        conditionalPanel(
            condition = "input.trimseqs =='No' && input.selecturseqs >0",
            div(
                id="leftpaneldiv",
                actionButton('cont_no_trim', 'CONTINUE'),
                br()
                )
        ), 
        # Option to remove any probelematic seqs from Haplotype Analysis Table. 
        # Server defines row click input. Panel present Continue 
        # PIE CHART SELECTION. Select from one of defined pie_choices column
        conditionalPanel(
            condition = "input.trimtheseq > 0 || input.cont_no_trim > 0",
            div( id="leftpaneldiv",
              div(
                br(), br(),
                h5("Step 7: Alternative links"),
                radioButtons('altlinks','Do you wish to view alternative links? ',
                             choices = c("Yes", "No"),
                             selected = "No",inline = TRUE)
              ),
                br(), br(),
                h5("Step 8. Create pie charts", actionLink("help7","?")),
                p(
                    id='leftpaneltext',
                    "Pie charts illustrate the proportions of a certain variable's
                    value per haplotype (node)"
                ),
                selectInput(
                    "pieselect", "Select pie chart variable", 
                    choices = piechoices, selected = "country",
                    multiple = FALSE),
                actionButton("piebtn", "SELECT & MAKE PIE"),
                br(),br()
            )
        ), 
        # CHOOSE AN EDGE TYPE AND GENERATE GRAPH AND LEGEND. 
        conditionalPanel(
            condition = "input.piebtn > 0",
            h5("Step 9. Choose how to display edges.", actionLink("help8",'?')),
            div(
                p(id = "leftpaneltext",
                  "Default is steps as shown as dots on edge line"
                ),
                radioButtons('steps', "Numeric edge label?",
                             choices = c("Yes", "No"), selected = "No",
                             inline = TRUE
                ),
                id = "leftpaneldiv",
                p(id = "leftpaneltext",
                  "Make sure Cytoscape is open before generating graph."),
                actionButton('generate', "GENERATE CYTOSCAPE GRAPH"),
                br(),
                br(),
                # Allows download of PNG legend
                downloadButton("downloadLegend", "Download a Legend for the graph.")
            ), 
            )
        ),
# MAIN PANEL: OUTPUTS ----------------------------------------------------------
   column( 
       width= 9,offset=3,
       style = ("height:100%;top:100px; position:absolute; overflow-y:scroll"),
       #fluidRow(
           id = "column2",
           # Loading... sign when shiny is busy/ computing 
           conditionalPanel(
               condition="$('html').hasClass('shiny-busy')",
               tags$div("Loading...")),
# divs for insertUI and remove UI. Allow tables to be created and deleted. ensure most recent table displayed only.
           #wellPanel(
           tags$div(id = "placeholder1"),
           tags$div(id = "placeholder1.1"),
           tags$div(id = "placeholder2"),
           tags$div(id = "placeholder3"),
           #),
           # OUTPUT MULTIPLE SEQUENCE ALIGNMENT Appear only when table confirmed. 
           conditionalPanel(
               condition = "input.finalconfirm >0",
               div(
                   p(id='tabletitle',
                     "Multiple sequence alignment of the selected sequences:"),
                   # show variant positions only. 
                   h6("View of MSA: Variant positions only."),
                   verbatimTextOutput("cleanss_varonly"),
                   tags$style("#cleanss_varonly { 
                              font-size:12px;overflow-y:scroll; 
                               max-height: 300px; background: ghostwhite; }"),
                   # show the base composition of each sequence
                   h6("Base Composition of each sequence: "),
                   verbatimTextOutput("basecomp"),
                   tags$style("#basecomp { 
                              font-size:12px;overflow-y:scroll; 
                              max-height: 300px; background: ghostwhite; }"),
                   # full msa
                   h6("View of MSA: complete "),
                   verbatimTextOutput("cleanssfullmsa"),
                   tags$style("#cleanssfullmsa { 
                              font-size:12px;overflow-y:scroll; 
                              max-height: 400px; background: ghostwhite; }"),
                   
                   div(style="text-align: right",
                       downloadButton("downloadOriginalData", 
                                      "Download original sequences")),
                   br()),
           ),
            conditionalPanel(
                condition = "input.selectgraphics = 'Yes'",
                tags$div(id = "placeholderimage", 
                         uiOutput("plotlyimage")
                         )
                ),

        conditionalPanel(
            condition= "input.selecturseqs > 0",
            wellPanel(textOutput("finaltbl"),
                      textOutput("finalseqs"),
                      style = ("background:ghostwhite; font-family:Courier New")),
            div(
                p(id="tabletitle","Information on invariant trailing gap regions"),
                p("Trailing gaps refer to the gaps that trail at the begining and the end 
                of sequences in the alignment. Trailing gaps get treated like N 
                characters during haplotype assignment. The table below displays 
                ranges of sequence where trailing gaps could be removed because 
                there are no other polymorphisms at these positions. You can use
                these values to help you to trim sequences to prevent any 
                haplotype separation by length. 
                See '?' for more details. "),
                verbatimTextOutput("wheretrailgaps")
            )
        ),
           # OUTPUT TRIM CHOICE Appear when confirm no trim, or confirm what trim. 
           conditionalPanel(
               condition = "input.trimtheseq > 0 || input.cont_no_trim > 0",
               verbatimTextOutput("trimmedssinfo"), # info on Seq length.
               tags$style("#trimmedssinfo { background: ghostwhite }"),
               div(style="text-align: right",
                   downloadButton("downloadTrimData", 
                                  "Download sequences following trim"),
                   br()
               ),
           ),
           # SUMMARY FINAL NETWORK: Following confirmation of removing any rows
           conditionalPanel(condition = "input.trimtheseq > 0 || input.cont_no_trim > 0",
                            br(),
                            p(id="tabletitle","Summary of final Haplotype Network:"),
                            div(
                                verbatimTextOutput("nhaplo1"),
                                h6("Summary information of the produced haplotype."),
                                p("Links refer to the number of edges in the graph.
                                  Steps refer to the distances/ mutational steps
                                  between haplotypes. "),
                                verbatimTextOutput('haploNet'),
                                tags$style("#haploNet { background: ghostwhite }"),
                                h6("Table showing which sequences have been assigned to 
                                   each haplotype"),
                                verbatimTextOutput("hapseq"),
                                div(style="text-align:right",
                                    downloadButton("downloadHaploSeqInfo", 
                                                   "Download Sequences in Haplotype table")
                                )
                            )
            ),  
           # SHOW PIE CHART TABLE  used for creating the pie charts. 
           conditionalPanel(condition = "input.piebtn > 0",
                            p(id="tabletitle","Frequency of Variable per Haplotype"),
                            verbatimTextOutput('freqhaptbl'),
                            tags$style("#freqhaptbl { background: ghostwhite }")
                            ), 
               ), 
        #    ),
)
        )

# SERVER -----------------------------------------------------------------------
server <- function(input, output) {
# HELP BUTTONS -----------------------------------------------------------------
# Clicking ? buttons opens modal dialog box.
# HELP BUTTON STEP 1. input BOLD search string 
    observeEvent(input$help1, {
        showModal(modalDialog(
            title = "How to Search BOLD",
            p("The Bold System Database can be searched via certain parameters including:", 
              strong("taxon, ids, bin, container, institution, researchers and geo")), 
            p(
            "You can search for multiple values of a parameter. Values must be 
            '|' delimited, indicating logical 'OR'", br(), br(),
            strong("Example of search format:", style="text-decoration: underline;"),
            br(),
            "To return records of the species 'Aurelia coerulea' and 
            'Aurelia aurita' the user would type: ", br(),
            em("taxon=Aurelia coerulea|Aurelia aurita"),
            ),
            p(
            "Searches can combine multiple parameters. 
            They must be '&' delimited indicating logical 'AND' clause.", br(),
            strong("Example of search format:",style="text-decoration: underline;"), 
            br(), em("taxon=Aurelia coerulea|Aurelia aurita&geo=Australia"), br(),
            "This search returns records of the species Aurelia coerulea and Aurelia aurita, 
            which are from Australia."), 
            strong("Further details on each search parameter are available ", 
                   tags$a("here",href="http://boldsystems.org/index.php/resources/api?type=webservices")),
            br(),
            p("Additional tips:"),
            tags$ul(
                tags$li("When searching for values made of multiple word e.g. 'Costa Rica', keep the space between words")
            ),
            easyClose = TRUE
        )
        )
    })
    # HELP BUTTON STEP 2: Select barcode 
    observeEvent(input$help2, {
        showModal(modalDialog(
            title = "Selecting a barcode gene",
            p(
            "BOLD mainly uses cytochrome c oxidase I (COI) as the main barcoding 
            gene for the animal kingdom. However researchers may submit other 
            genes as  barcodes. For  plants and fungi it is common for multiple 
            barcoding genes to be submitted by researchers. 
            Instances where multiple barcoding genes are available, you will 
            need to select the gene you wish to use. "
            ),
            easyClose = TRUE
        )) 
    })
    # HELP BUTTON 3: Greedy search
    observeEvent(input$help3, {
        showModal(modalDialog(
            title = "What is a Greedy Search? ",
            p(
            "Barcode Index Numbers (BINs) are barcodes grouped based on similarity. 
            Barcodes get grouped algorthimically by BOLD. 
            BINs often correspond to species. However species showing significant 
            intraspecific diveristy may get separated into multiple BINs. Closely 
            related species may have their specimens binned together due to low 
            intraspecific sequence divergenece.
            Greedy search looks at the BINs in current table and adds any 
            additional specimens from these BINs to the current dataset.", 
            br(), 
            "This feature may be useful to indivduals doing species based inital 
            searches, as this feature will retrieve other specimens that have 
            been 'binned' with the chosen species. "
            ),
            easyClose = TRUE
        )) 
    })
    
    observeEvent(input$help4,{
        showModal(modalDialog(
            title="Filtering the table allows you to retain only the records you want",
            p(
                "There are two tools you can use for filtering the 'Simple' and
                the 'Advanced'. The Simple tool is quicker to use and is suitable for 
                users who have simple filtering queries, if for example you only need to
                filter by one column. The column you can filter with are: Bin, species, 
                sub-species, country, province, region, institution and identifier. 
                If selecting values from multiple columns these will be combined togetehr 
                using AND logic e.g: clicking 'Canada' and BIN:ABD656 will remove specimens  
                from BIN:ABD656  that are from Canada. ",
                br(),
                "If your filter needs to use other columns, or requires logic 
                such as: select records from Canada OR from BIN:ABC6756 you will need to
                use the Advanced tool. Here you can define more complex filters, by constructing
                the appropriate conditional expression. There is a detailed guide to help you construct." 
                
            ),
            easyClose = TRUE
        ))
    })
    
    
    # HELP BUTTON 4a: Filtering the table- advanced
    observeEvent(input$help4a, {
        showModal(modalDialog(
            title = "Advanced Filter tool ",
            p(
            "Users can type in a conditional expression 
            (expression that evaluates to TRUE or FALSE) decalring logical rules 
            to filter the table records with.
            The logical rules used in this app follow the format of those used by the", 
            tags$a(
                href= "https://dplyr.tidyverse.org/reference/filter.html", 
                "R package: dplyr and its filter()"
                ),
            "function. The app will return the rows where the user's given cases 
            are TRUE."),
            strong("How to define logical rules:"),
            p("Any string values should be surrounded by ''.", br()),
            strong("Examples of filter function: (not extensive)"),
            p(
            strong("== (equals), != (not equal to)"), br(), 
            "country=='Canada', Selects all records from Canada", br(),
            "country!='Canada' Selects all records from countries other than Canada"
            ),
            p(
            strong(">, >=, etc: for numerical values."), br(),
            "recordID>40000, Selects all records with recordID's greater than 40000"
            ),
            p(
            strong("&: logical AND"),
                "Useful for combining multiple logical rules.", br(),
                " bin_uri=='BOLD:ABC567'&country=='Peru', Selects only records from the 
                bin BOLD:ABC567 that are from Peru"
             ),
            p(
            strong("|: logical OR."), br(), 
            "institution_storing=='Manchester Univeristy'|country=='England.  
            Selects records that are either from Manchester Univeristy or from England."
            ),
            p(
            strong("%in% c(): select any value in the list c()"), br(),
            "country %in% c('Peru','Canada'),  Selects any record from Peru or Canada"
            ),
            p(
            strong("!: put in front of a logical rule to negate condition specified"), 
            br(), 
            "!(country %in% c('Peru','Canada')), Selects all records that are not from Peru or Canada"),
            p(
            strong("is.na(): Selects any records with NA in this column, "), br(), 
            "is.na(species_name), Selects any records with NA in the species_name column", 
            br(),
            "is.na(species_name), Selects any records that do not have NA in the species_name column"
            ),
            easyClose = TRUE
        )) })
    # HELP BUTTON 5: Remove any unwanted sequences. 
    observeEvent(input$help5,{
        showModal(modalDialog(
            title="Select any Sequences you wish to remove.",
            p(
                "If following the sequence quality you wish to remove some sequences
                from the dataset, you can remove them here. If you do not want to remove
                any sequence, leave blank and click Continue"
            ),
            easyClose = TRUE
        ))
    })
    
    
    # HELP BUTTON 6: Trim seqs
    observeEvent(input$help6, {
        showModal(modalDialog(
            title = "Trimming sequences ",
            p(
            "This function allows you to trim the sequence. This means you can 
            choose to only use a certain portion of the gene sequence to analyse
            and use to build a haplotype network. You may wish to remove ends
            of the sequence to reduce the chance of haplotypes being separated due 
            to length. For example by default the haplotype assignment algorithm would spearate 
            sequences: ACGCTTCG, ACGCTTCG, into one haplotype and ACGCT into another.", br(),
            "The invariant trailing regions show you the trailing gap regions you can remove from 
            the ends of the alignment without removing any positions of sequence of variation
            which may be deterministic of haplotype. "),
            easyClose = TRUE
        )) 
    })
  
    # HELP 7: PIE CHARTS
    observeEvent(input$help7, {
        showModal(modalDialog(
            title = "Using pie charts in haplotype networks",
            p(
            "When creating a haplotype network pie charts are commonly placed on
            each haplotype's node. The pie charts convey information about the 
            specimens that make up a certain haplotype. For example they could 
            detail how many specimens in each haplotype come from a certain country.",
            br(), br(),
            "In this application you can select one of the column variables to 
            use to create pie charts with. "
            ),
            easyClose = TRUE
        )) 
    })
    # HELP BUTTON 8: EDGE SELECTION.
    observeEvent(input$help8, {
        showModal(modalDialog( 
            size='l',
            title = "Edges of the Haplotype Network. ",
            p(
            "This application uses the steps/ distance between each haplotype to
            label edges. Steps are the number of nuclotide differences between 
            the 2 haplotype nodes that the edge joins. For example the number of 
            steps between these two sequences: ATTGCTC and ATTGCAC is 1. ", br(), 
            "This application allows users to either display the steps as dots 
            or as numeric labels. We recommend using numeric labels for large 
            haplotype networks or those with edge steps greater than 10, 
            for clarity. "
            ),
            strong('Edge examples:'),
            p(
            "Numeric label", br(),
            img(src='numericlabel.png', align = "center",  height="50%", width="100%"), 
            br(), "Steps as dots", br(),
            img(src='stepsasdots.png', align = "center",  height="50%", width="100%")
            ),
            easyClose = TRUE
        )) 
    })
    
    # BOLD TABLE CREATION-----------------------------------------------------------------
    # Error Function. For checking a users input string to display validation message (DNA function only)
    error_boldsearchstr <- function(boldsearchstr) {
        if (boldsearchstr == "") {
            "Error: Please enter a search string"
        } else if (grepl("taxon|ids|bin|container|institutions|researchers|geo",
                         boldsearchstr) == FALSE) {
            " Click the ? button for more help"
        } else if (grepl("=", boldsearchstr) == FALSE) {
            "Click the ? button for more help on formatting"
        } else {
            NULL
        }
    }
    # SEARCH BOLD AND CREATE TABLE 
    # tryCatch used to catch input errors and return a useful message based on issues 
    # presented in string. 
    # If unable to diagnose issue, prompts user to check instructions.       
    original_table <- eventReactive(input$boldsearchbtn, {
        tryCatch({
            shiny::validate(error_boldsearchstr(input$boldsearchstr))
            data.frame(recordTableCreator(input$boldsearchstr))
        },
        error = function(e) {
            # null string error
            if (input$boldsearchstr == "") {
                showNotification(paste0("ERROR. Please enter a Search String."),
                                 type = "error")
                # no search parameter defined
            } else if (grepl(
                "taxon|ids|bin|container|institutions|researchers|geo",
                input$boldsearchstr
            ) == FALSE) {
                showNotification(
                    paste0(
                    "Check your input. Ensure the value you are searching for
                    exists within the BOLD database.Ensure that you have specified
                    a BOLD parameter to search such as: bin, taxon or geo etc.
                    Click the ? button for help on how to structure your query"
                    ),
                    type = "error"
                )
                # no = used
            } else if (grepl("=", input$boldsearchstr) == FALSE) {
                showNotification(
                    paste0(
                    "Check the format of the string. It looks like you
                    have missed out an =. Is it in a format similar to
                    bin=BOLD:AAA2176|BOLD:AAA2178&geo=France ?
                    Click the ? button on Step 1 for more help on formatting"
                    ),
                    type = "error"
                )
                # any other errors
            } else {
                showNotification(
                    paste0(
                        "Check your input. Ensure the value you are searching for
                        exists within the BOLD database.
                        Click the ? button on Step 1 for more help. "
                    ),
                    type = "error"
                )
            }
        })
    })
    # SEARCH FOR SEQUENCE
    bold_seqs <- eventReactive(input$boldsearchbtn, {
        shiny::validate(error_boldsearchstr(input$boldsearchstr))
        dnaSSCreator(input$boldsearchstr)
    })
    
    #Remove any records which dont have any sequene available
    bold_table <- eventReactive(original_table(), {
        tryCatch(
            cleanTable(original_table(), bold_seqs()),
            error = function(e) {
                return()
            },
            warning = function(w) {
                return()
            }
        )
    })
    
    # RENDER OUTPUT OF BOLD TABLE
    output$boldtable <- renderDT(
        setcolorder(bold_table(), colorders),
        server = FALSE,
        class = 'table-condensed table-striped',
        extensions = list('FixedColumns' = NULL,
                          'Buttons' = NULL),
        options = table_options

    )

    # RENDER TEXT: N ROW IN BOLD TABLE
    output$boldtableinfo <-
        renderText(paste0("The table originally downloaded from BOLD has: ", nrow(original_table()), " records. 
                          Records with sequence available to analyse : ", nrow(bold_table())))
    # CLEAR ANY PREVIOUS TABLES- allows user to go back to search without reloading app.
    observeEvent(input$boldsearchbtn, {
        removeUI(selector = "div:has(> #boldtable)", immediate = TRUE)
        removeUI(selector = "div:has(> #updatedtable)", immediate = TRUE)
        removeUI(selector = "div:has(> #greedytable)", immediate = TRUE)
        removeUI(selector = "div:has(> #filtertbl)", immediate = TRUE)
    })
    # UPON SEARCH CREATE INSERT UI INTO DIV. ALLOWS INSERTION AND REMOVAL, showing only most recent table. 
    observeEvent(input$boldsearchbtn, {
        insertUI(
            selector = "#placeholder1",
            where = "afterEnd",
            immediate = TRUE,
            ui = tags$div(
                style = 'padding-left:5px; margin-right=10px',
                tags$div(id = 'tabletitle',
                         textOutput("boldtableinfo")),
                DTOutput("boldtable")
            )
        )
    })
    #  GENE SELECTION  ----------------------------------------------------------
    # CREATE GENE SELECTOR TABLE. uses values from downloaded sequence set. needs to re-generated with each bold search
    # CREATE OPTIONS: NAME (N RECORDS)
    gene_table <-
        reactive({
            data.frame(geneMarkerAvailable(bold_seqs()))
        })
    gene_as_list <- reactive({
        genelist <- list()
        for (i in 1:nrow(gene_table())) {
            genelist <- c(genelist, paste0(gene_table()$genemarker, " (",
                                           gene_table()$Freq, ")"))
        }
        return(genelist)
    })
    # ADD GENE SELECTOR CONTROL IN SIDE PANEL 
    output$geneselector <- renderUI({
        tagList(
            h5("Step 2. Select Barcode", actionLink("help2", label = "?")),
            selectInput(
                "selectedgene",
                label = "Select a barcoding gene to use:",
                choices = gene_as_list(),
                multiple = FALSE,
                selected = TRUE
            ),
            actionButton("selectagene", label = "Select Gene"),
            br(),
            br(),
        )
    })
    # CLEAN INPUT TO KEEP GENE NAME ONLY.
    selectgene <- eventReactive(input$selectagene, {
        splitrow <- unlist(strsplit(input$selectedgene, " ", fixed = TRUE))
        return(splitrow[1])
    })
    # UPDATE SEQS AND TABLE TO KEEP ONLY THOSE WITH SELECTED GENE.
    # Sort sequences
    selected_seqs <- eventReactive(input$selectagene, {
        geneMarkerSelection(bold_seqs(), selectgene())
    })
    # Sort the table
    updated_table <- eventReactive(input$selectagene, {
        cleanTable(bold_table(), selected_seqs())
    })
    # RENDER NEW TABLE WITH ONLY SELECTED GENE
    output$updatedtable <- renderDT(
        setcolorder(updated_table(), colorders), 
        server = FALSE,
        class = 'table-condensed table-striped',
        extensions = list(
            'FixedColumns'= NULL,
            'Buttons' = NULL),
        options = table_options
    )

    # RENDER THE UPDATED TABLE TITLE
    output$selectgeneinfo <-
        renderText(paste0(
            "Table following gene selection has : ",
            nrow(updated_table()),
            " records. "
        ))
    
    # REMOVE THE OLD TABLE 
    observeEvent(input$selectagene, {
        removeUI(selector = "div:has(> #boldtable)", immediate = TRUE)
    })
    # REPLACE WITH UPDATED TABLE 
    observeEvent(input$selectagene, {
        insertUI(selector = "#placeholder1",
            where = "afterEnd",
            immediate = TRUE,
            ui = tags$div(
                div(id = 'tabletitle',
                    textOutput("selectgeneinfo")),
                DTOutput("updatedtable")
            )
        )
    })
    # GREEDY SEARCH -------------------------------------------------------------
    # GENERATE THE NEW URLS NEEDED FOR GREEDY SEARCH. URl combine all current bins in dataset
    # generates speciman and sequence urls in list. 
    greedy_urls <- reactive({
        if (input$greedycheckbox == "Yes") {
            greedySearchUrls(updated_table())
        }
    })
    # if selected: get seqs from Greedy Seach and append to dataset, else keep selected_seqs
    greedy_seqs <- eventReactive(input$continuegreedy, {
        if (input$greedycheckbox == "Yes") {
            # use sequence url to fetch.
            greedyseqs <- readDNAStringSet(greedy_urls()[[2]])
            # keep only sequences with the chosen gene 
            greedyseqs <-
                geneMarkerSelection(greedyseqs, selectgene())
            for (i in 1:length(selected_seqs())) {
            # join original and greedy seq, removing duplicates based on name.
                if (!(names(selected_seqs())[i] %in% names(greedyseqs))) {
                    greedyseqs <- c(greedyseqs, selected_seqs()[i])
                }
            }
            return(greedyseqs)
        # if greedy search not selected keep the current sequences. 
        } else {
            return(selected_seqs())
        }
    })
    #if selected, get greedy table , and append new records. else keep updated_table
    greedy_table <- eventReactive(input$continuegreedy, {
        if (input$greedycheckbox == "Yes") {
            # get new table, bind to old, remove duplicate records. 
            joined_table <-
                read.csv(greedy_urls()[[1]], sep = '\t', header = TRUE, na.strings = '') %>%
                rbind.data.frame(updated_table()) %>%
                distinct()
            # clean table to remove any records that don't have seq.
            cleanTable(joined_table, greedy_seqs())
        } else {
            updated_table()
        }
    })
    
    # RENDER GREEDY TABLE TITLE 
    output$greedytableinfo <- renderText(if (input$greedycheckbox == "Yes") {
        paste0("Table following greedy search has : ",
               nrow(greedy_table()),
               " records. ")
    } else{
        paste0("Greedy search not selected. Current table has : ",
               nrow(greedy_table()),
               " records. ")
    })
    
    output$greedytable<-renderDT(
        setcolorder(greedy_table(), colorders
        ),
        server = FALSE,
        class = 'table-condensed table-striped',
        extensions = list(
            'FixedColumns'= NULL,
            'Buttons'= NULL),
        options = table_options
    )

    # remove old table                              
    observeEvent(input$continuegreedy, {
        removeUI(selector = "div:has(> #updatedtable)", immediate = TRUE)
        removeUI(selector="div:has(> #greedytable)", immediate = TRUE)
        #remove after things if re ran
        removeUI(selector = "div:has(> #greedywSP)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_ad)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_non)", immediate = TRUE)
    })
    # add in new table - greedy 
    observeEvent(input$continuegreedy, {
        insertUI(selector = "#placeholder1", where = "afterEnd", immediate = TRUE,
                 ui = tags$div(
                     br(),
                     div(id = 'tabletitle',
                         textOutput("greedytableinfo"), ),
                     DTOutput("greedytable"),
                     textOutput("y11"),
                     br(), br()
            )
        )
    })
    #  FILTER   TABLE ----------------------------------------------------------
    
    # If the user specifies to use the simple search tool- render a table with a 
    # SerachPane button.
    observeEvent( input$filtertype,{
        if (input$filtertype == 'Simple') {
            # render the greedy table with a SearchPane added
            output$greedywSP <- DT::renderDataTable(
                setcolorder(greedy_table(), colorders),
                server = FALSE,
                extensions = list( 'FixedColumns'= NULL, 'Select'=NULL, 
                                   'SearchPanes'= NULL, 'Buttons'=NULL),
                selection = 'none',
                options= list( columnDefs = list(
                        # columns to show in filter
                        list(searchPanes = list(show = TRUE,className='myclass'), 
                             targets=c(2,3,4,10,11,12,13,14)),
                        # columns to hide 
                        list(searchPanes = list(show = FALSE), 
                             targets=c(1,5,6,7,8,9,15:68))),
                    language= list(searchPanes=list(collapse= 'Select Filter')),
                    scrollX = TRUE, scrollY = 500,
                    fixedColumns = list(leftColumns = 4),
                    searchPanes=list(layout= 'columns-3', 
                                     emptyPanes= 'There are no panes to display.'),
                    dom = 'Bfrtip',
                    buttons = c('searchPanes')
                )
            )
            # print to user the rows that have been selected
            observeEvent(input$greedywSP_rows_all, {
                # message if user didnt select any rows
                if (length(input$greedywSP_rows_all) == nrow(greedy_table())) {
                    output$rows_f_searchpane <- renderText("You have not applied any filters.
                        You have not selected any rows to remove. ")
                    output$SP_table_title <- renderText("Original table: no rows removed")
                } else{
                    # message is the user selected rows
                    output$rows_f_searchpane <- renderText(paste0(
                        "Filters applied. ",
                        "You have selected to remove the following rows: ",
                        toString(input$greedywSP_rows_all) ))
                    output$SP_table_title <- renderText("Rows you selected for removal")
                }
            })
# insert Ui: contains searchPane filtered table and appropriate headers
            insertUI(
                selector = "#placeholder2",
                where = "afterEnd",
                immediate = TRUE,
                ui = tags$div( style = 'padding-left:5px; margin-right=10px',
                    tags$div(
                        # title of the table
                        div(id = 'tabletitle', textOutput("SP_table_title")),
                        # info on number of rows removed
                        verbatimTextOutput("rows_f_searchpane"),
                        tags$style(
                            "#row_searchpane {
                             font-size:12px;overflow-y:scroll;
                             background: ghostwhite }"
                        ),
                        # ouput of the table with a searchPane
                        DTOutput("greedywSP"))
                )
            )
            removeUI(selector = "div:has(> #table_ad)", immediate = TRUE)
            removeUI(selector = "div:has(> #table_non)", immediate = TRUE)
            removeUI(selector = "div:has(> #filtertbl)", immediate = TRUE)
        }
    })
    
    # IF USER SELECTS ADVANCED SEARCH OPTION: following runs. 
    # if a filter string is given- select the indexes of the rows the user has asked to remove. 
    
    # indexes of the rows to be removed. If no search string given: no indexes
    row2removead <- eventReactive(input$filterbtn,{
        if(trimws(input$filterstatement, which = "both")==""){
            return(NULL)
        } else{
            df_windex <- mutate(greedy_table(), IDX = 1:n())
            df_unwantedrows <- filterTable(df_windex,trimws(input$filterstatement, which = "both"))
            index_extract <- unlist(df_unwantedrows$IDX)
            if(length(index_extract)==0){
                return(NULL)
            } else{
                return(index_extract)
            }
        }
    })
    # tell user rows they chose to remove/ they didn't choose any
    rowchoosed <- eventReactive(input$filterbtn,{
      if(trimws(input$filterstatement, which = "both")=="") {
        string<- paste0("You did not select any rows to remove.",
          "Type in a filter statement and click 'Filter'",
          "For more help click the '?' in the control panel.")
        return(string)
      } else if(trimws(input$filterstatement, which = "both")!="" && 
                is.null(row2removead())) {
        string<- paste0("No filter applied. ",
               "Possible error in your filter statement, or there were no records to remove ")
        return(string)
      } else{
        string<- paste0(" The rows you have selected to be removed are: ",toString(row2removead()))
        return(string)
      }
      
    })
    output$ad_row<- renderPrint({rowchoosed()})

    # table with the rows (advanced) removed. if users filter string resulted in no rows being 
    # removed. returns the original data set
    advanced_filter_table <- reactive({
        if(is.null(row2removead())) {
            return(greedy_table())
        }
        else{
            return(greedy_table()[-row2removead(),])
        }
    })
    # render the dataset WITH THE ROWS REMOVED (shown at final confirm)
    output$ad_table_filt <- renderDataTable(advanced_filter_table())
    # Show user the ROWS THEY SELECTED FOR REMOVAL. (shown at check1)
    output$row_removed<- renderDataTable(greedy_table()[row2removead(),])
    
    # if user selected advanced- 
    observeEvent(c(input$filtertype), {
        if (input$filtertype == 'Advanced') {
            removeUI(selector = "div:has(> #greedywSP)", immediate = TRUE)
            removeUI(selector = "div:has(> #table_ad)", immediate = TRUE)
            removeUI(selector = "div:has(> #table_non)", immediate = TRUE)
            removeUI(selector = "div:has(> #filtertbl)", immediate = TRUE)
        } 
    }) 
    
    observeEvent(input$filterbtn, {
        removeUI(selector = "div:has(> #greedywSP)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_ad)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_non)", immediate = TRUE)
        removeUI(selector = "div:has(> #filtertbl)", immediate = TRUE)
        insertUI(
            selector = "#placeholder2", where = "afterEnd", immediate = TRUE,
            ui = tags$div(
              tags$div(id = 'table_ad',
                div(id = 'tabletitle', 
                    p("Advanced filter "),
                    ),
                verbatimTextOutput("ad_row"),
                DTOutput("row_removed")
            )
            )
        )
    })
    # if the user selected no tool:
    observeEvent(c(input$filtertype,input$continuegreedy),{
        if(input$filtertype == 'None'& input$continuegreedy >0){
            removeUI(selector = "div:has(> #greedywSP)", immediate = TRUE)
            removeUI(selector = "div:has(> #table_ad)", immediate = TRUE)
            removeUI(selector = "div:has(> #table_non)", immediate = TRUE)
            removeUI(selector = "div:has(> #filtertbl)", immediate = TRUE)
            insertUI(
                selector = "#placeholder2",
                where = "afterEnd",
                immediate = TRUE,
                ui = tags$div(tags$div(
                    id = 'table_non', 
                    wellPanel(
                        p("No rows will be removed. 
                          Click 'see table' to inspect the final dataset"))))
            )
        }
    })
    
    
    
    # following the check1.
    filter_tbl<-  eventReactive(input$check1,{
            if (input$filtertype == 'Simple' & length(input$greedywSP_rows_all) == nrow(greedy_table())){
                return(greedy_table())
            }
            if (input$filtertype == 'Simple' & length(input$greedywSP_rows_all) != nrow(greedy_table())){
                return(greedy_table()[-as.numeric(input$greedywSP_rows_all), ])
            }
            if (input$filtertype == 'Advanced') {
                return(advanced_filter_table())
            }
            if (input$filtertype == 'None') {
                return(greedy_table())
            }
    })
    
    # Render Filtered Table
    output$filtertbl <- renderDT(
        setcolorder(filter_tbl(), colorders),
        server = FALSE,
        class = 'table-condensed table-striped',
        extensions = list('FixedColumns' = NULL,'Buttons' = NULL),
        options = table_options
    )
    
    # Show the user the final table. before rendering all the other things
    observeEvent(input$check1, {
        removeUI(selector = "div:has(> #greedywSP)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_ad)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_non)", immediate = TRUE)
        insertUI(selector = "#placeholder2", where = "beforeBegin",immediate = TRUE,
                 ui = tags$div(
                     br(), br(),
                     div(id = 'tabletitle',
                         p("Dataset following your filtering selection")),
                     DTOutput("filtertbl")
                 )
        )
    })
    # notification if user doesn't select anything through the SearchPane or advances search. 
    # tells them that the original table with no rows removed will be returned  
    observeEvent(input$check1, {
        if (input$filtertype == 'Simple' &
            length(input$greedywSP_rows_all) == nrow(greedy_table())) {
            showNotification(paste0("You did not select any rows to remove. The original table has been returned"), 
                             type = "error")
        }
        if(input$filtertype=='Advanced' &
           trimws(input$filterstatement, which = "both")=="") {
            showNotification(paste0("You did not select any rows to remove. 
                                    The original table has been returned"),
                             type = "error")
        }
    })
    
    # once user is happy, remove all the previous tables. 
    observeEvent(input$finalconfirm, {
        removeUI(selector = "div:has(> #greedywSP)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_ad)", immediate = TRUE)
        removeUI(selector = "div:has(> #table_non)", immediate = TRUE)
        removeUI(selector = "div:has(> #filtertbl)", immediate = TRUE)
    })
    
    # Once user has confirmed the that they are happy with table, 
    # remove all previous tables and keep only final filtered. 
    observeEvent(input$finalconfirm, {
        insertUI(selector = "#placeholder1", where = "beforeBegin",immediate = TRUE,
                 ui = tags$div(
                     div(id = 'tabletitle',
                         p("Final Dataset")),
                     DTOutput("filtertbl"))
                )
        removeUI(selector = "div:has(> #greedytable)", immediate = TRUE)
    })
    
    # CLEAN SEQUENCES ---------------------------------------------------------
    # Following the filtering of records, filter the sequences to keep only matching ones.
    filter_seqs <- eventReactive(input$finalconfirm,{
        selectSeqs(filter_tbl(), greedy_seqs())
        })
    # CLEAN SEQS remove end gaps, align using Clustal W and remove Ns in non-informative positions. 
    #allow user to download the original seqs in fasta. 
    removed_eg <- reactive({
        removeEndGaps(filter_seqs())
    })
    #to print
    clean_ss_msa <- reactive({
        msa(removed_eg(), "ClustalW")
    }) 
    # convert back to SS for further use
    clean_ss <- reactive({
        as(clean_ss_msa(), "DNAStringSet")
    })
        
    clean_ssalview<-reactive({
        removenonvar(clean_ss())
    })
    
    # RENDER THE MSA. 
    output$cleanssfullmsa<-renderPrint(
        print(clean_ss_msa(), show='complete', halfNrow=NA)
    )
    output$cleanss_varonly <- renderPrint(printMultipleAlignment(clean_ssalview()))
    # CREATE DOWNLOAD HANDLER TO ALLOW DOWNLOAD OF SEQS IN FASTA FORMAT. 
    output$downloadOriginalData <- downloadHandler(
        filename = function() {
            paste0("Sequences.txt")
        },
        content = function(file) {
            writeXStringSet(removed_eg(), filepath = file, format = 'fasta')
        }
    )
    
    #show user base composition of each seq
    base_comp <- reactive({baseComposition(clean_ss())})
    output$basecomp <- renderPrint(base_comp())
    
    ## Render an image ---- 
    observeEvent(input$selectgraphics,{
        if(input$selectgraphics =="Yes"){
    
    matrix_clean_ss <- reactive({as.matrix(clean_ss())})
    ranges <- reactiveValues(width_im = NULL,height_im=NULL)
    ranges$width_im <- reactive(ncol(matrix_clean_ss())*10)
    ranges$height_im<- reactive({
        if(nrow(matrix_clean_ss()) < 150){
            return(800)
        } else if (nrow(matrix_clean_ss()) > 150 & nrow(matrix_clean_ss()) < 300 ) {
            return(1500)
        } else{
            return(nrow(matrix_clean_ss())*10)
        }
    })

    ms <- reactive({
        alph <- c("A"=1, "C"=2, "G"=3, "T"=4, "-"=5, "N"=6, "R"=7,"S"=8,
                  "W"=9,"K"=10,"M"=11,"B"=12,"D"=13,"H"=14,"V"=15)
        ms <- matrix(alph[matrix_clean_ss()], ncol = ncol(matrix_clean_ss()), byrow = FALSE)
        rownames(ms) <- rownames(matrix_clean_ss())
        return(ms)
    })
    mm <- reactive({
        data.frame(do.call(rbind, lapply(rownames(ms()), rep, ncol(matrix_clean_ss()))))
    })

    output$testplot <- renderPlotly({

        heatmaply(ms(), custom_hovertext = mm(),
                  cellnote = matrix_clean_ss(),cellnote_size = 6,
                  cellnote_textposition = "middle center",
                  fontsize_col = 6,grid_gap = 1,grid_size = 0.01,
                  show_dendrogram = c(FALSE, FALSE),
                  Rowv=NULL, Colv=NULL, color=rainbow(15),
                  hide_colorbar=TRUE,plot_method = "plotly")
    })

    output$plotlyimage <- renderUI(
      div(
        h6("Interactive view of MSA"),
        tags$div(class="superbigimage",
                 plotlyOutput("testplot", width=ranges$width_im(), height=ranges$height_im()))
    )
    )
        }
    })

    # create ist of sequence names- ID only.
    seq_name_as_list <- reactive({
        seqlist<-lapply(names(clean_ss()), function(x) unlist(strsplit(x, '|',fixed=TRUE)))
        seqlist<-sapply(seqlist, "[[",1)
        return(seqlist)
    })
    # render controls which allow user to select any sequences for removal. 
    output$sequenceremove <- renderUI({
        tagList(
            h5("Step 5. Select any sequence you wish to remove.",actionLink("help5","?")),
            selectizeInput(
                "selectseqtoremove",
                label="Select the sequences you wish to remove from the final dataset. 
                Leave blank to remove none.",
                choices=seq_name_as_list(),
                multiple = TRUE,
            ),
            actionButton("selecturseqs", label = "CONTINUE"),
        )
    })
    
    final_seqs <- eventReactive(input$selecturseqs,{
        if(is.null(input$selectseqtoremove)){
            return(clean_ss())
        } else{
            removeSeqs(input$selectseqtoremove, clean_ss())
        }
    })
    
    final_tbl <- reactive({
        cleanTable(filter_tbl(),final_seqs())
    })    
    
    # RENDER CONFIRMATION OF REMOVAL.
    output$finaltbl <-
        renderPrint(paste0(
            "Final number of records in table to be used for haplotype network: ",
            nrow(final_tbl())
        ))
    output$finalseqs <-
        renderPrint(paste0(
            "Final number of Sequences to be used for haplotype network: ",
            length(final_seqs())
        ))
    

    # TRIM SEQUENCES ------------------------------------------------------------------
    # If trim selected, cut sequence between inputted start and end, 
    
    # help user decide where to cut. 
    
    where_trail_gaps <- reactive({
        numbz<-gappos(final_seqs())
        intervals<- R.utils::seqToIntervals(numbz)
        return(intervals[c(1,nrow(intervals)), ])
    })

    output$wheretrailgaps <- renderPrint(where_trail_gaps())
    
    trimmed_seqs <- reactive({
        if (input$trimseqs == 'No') {
            return(final_seqs())
        }
        if (input$trimseqs == 'Yes') {
            input$trimtheseq
            trimSeqs(final_seqs(), isolate(input$trimstart), isolate(input$trimend))
        }
    })
    
    # RENDER LENGTH OF SEQS FOLLOWING TRIM
    output$trimmedssinfo <-
        renderPrint(paste0("Length of sequences in the final alignment: ",
                           nchar(trimmed_seqs()[[1]])
        ))
    # DOWNLOAD HANDLER TO DOWNLOAD FASTA OF SEQS FOLLOWING TRIMMING 
 
    output$downloadTrimData <- downloadHandler(
        filename = function() {
            paste0("Sequences_after_trim.txt")
        },
        content = function(file) {
            writeXStringSet(trimmed_seqs(), filepath = file, format = 'fasta')
        }
    )
    
    # HAPLOTYPE TABLE -------------------------------------------------------------
    # Get names of species that have only one representative record in the dataset 
    #unique_sp_name <- reactive({ unique_Species_Names(filter_tbl()) })
    
    # make Initial haplotype 
    haplo1 <- reactive({
        makeHaploType(trimmed_seqs())
    })
    
    # RENDER NUMBER OF HAPLOTYPES FOUND INITIALLY. 
    output$nhaplo1<-renderPrint({
        paste0("Number of haplotypes found: ", dim(haplo1())[[1]])
    })
    
    # CREATE FINAL HAPLOTYPE AND NETWORK -----------------------------------------
    # MAKE FINAL HAPLOTYPE THAT WILL BE USED.
    haplo_final <- reactive({
        makeHaploType(trimmed_seqs())
    })
    
    haplo_seq_info <- reactive({nameinHaplotype(haplo_final(), trimmed_seqs())})
    output$hapseq<-renderPrint(haplo_seq_info())

    output$downloadHaploSeqInfo <- downloadHandler(
        filename = function() {
            paste0("seqinhaplo.txt")
        },
        content = function(file) {
            write.csv(haplo_seq_info(), file = file )
        }
    )
    
    # make the haploNet 
    haplo_Net <- reactive({
        makeHaploNet(haplo_final())
    })
    # RENDER the output summary of haplotype network.
    output$haploNet <- renderPrint(haplo_Net())
    
    # MAKE IGRAPH
    new_ig <-
        reactive({
          if(input$altlinks=='Yes'){
            as.igraph(haplo_Net(), directed = FALSE, use.labels = TRUE, 
                      altlinks = TRUE)
          } else{
            as.igraph(haplo_Net(), directed = FALSE, use.labels = TRUE, 
                      altlinks = FALSE)
          }
            
        })
    # PIE CHARTS ---------------------------------------------------------------
    freq_index <-
        reactive({
            which(colnames(final_tbl()) == input$pieselect)
        })
    # CREATE PIE CHART TABLE- table with each pie chart factor and how many records/ haplotype match it.
    freq_hap_tbl <- eventReactive(input$piebtn, {
        fmatrix <-
            freq_Matrix(final_seqs(), final_tbl(), freq_ind = freq_index())
        fhaptbl <- freq_Hap_table(fmatrix, haplo_final())
        fhaptbl <- `row.names<-`(fhaptbl, attr(haplo_Net(), "labels"))
        return(fhaptbl)
    })
    # RENDER OUTPUT OF THE PIE CHART TABLE
    output$freqhaptbl <- renderPrint(freq_hap_tbl())
    # add pie chart to the igraph and adjust other properties such as steps and node labels
    final_att_graph <- reactive({
      if(input$altlinks=='Yes'){
        pie_Cols_add(freq_hap_tbl(), new_ig()) %>%
          igraphPropaltlink(haplo_Net())
          
      }
        pie_Cols_add(freq_hap_tbl(), new_ig()) %>%
            igraphProperties(haplo_Net())
    })
    
    # STORE NAMES OF THE ORIGINAL EDGES.- will need to be removed if user chooses 'dots' edges. 
    original_edges <-
        reactive({
            as.list(attr(E(final_att_graph()), "vnames"))
        })
    # add subnodes/dots  to the graph.     
    ig_w_subnodes <- reactive({
        # user selects to have dot edges
        if (input$steps == "No") {
            t <- createSubNodes(original_edges(), final_att_graph())
            return(delete_edge_attr(t, 'steps'))
        }
        # user selects to have numeric edge labels
        if (input$steps == "Yes") {
            return(final_att_graph())
        }
    })
    # add pie chart column attributes 
    piecols <-
        reactive({
            paste0(gsub("[[:space:]]|[[:punct:]]", "_", colnames(freq_hap_tbl())))
        })
    # select the colours for pie chart. 
    colorz <- reactive({
        #rainbow(length(piecols()))
      randomcoloR::distinctColorPalette(k = length(piecols()))
    })
    # COMMANDS FOR CYTOSCAPE GRAPH GENERATION 
    # Trycatch warns user if cytoscape is not open 
    observeEvent(input$generate, {
        tryCatch({
            cytoscapePing()
            createNetworkFromIgraph(ig_w_subnodes())
            setNodeCustomPieChart(piecols(), slot = 3, colors = colorz())
            if (input$steps == "Yes") {
                setEdgeLabelMapping('steps')
            }
            if (input$steps == "No") {
                setEdgeLabelDefault("", 'default')
                updateStyleMapping('default',
                                   mapVisualProperty('node fill color', 'color', 'p'))
            }
            setNodeShapeDefault('ELLIPSE')
            lockNodeDimensions(TRUE)
            updateStyleMapping('default', mapVisualProperty('node size', 'size', 'p'))
            updateStyleMapping('default',
                               mapVisualProperty('node label', 'finallabel', 'p'))
            layoutNetwork('force-directed')
        },
        warning = function(w) {
            showNotification(paste0(
                "Cytoscape is not open. Please open cytoscape on your computer "
            ),
            type = "error")
        },
        error = function(e) {
            showNotification(paste0(
                "Cytoscape is not open. Please open cytoscape on your computer"
            ),
            type = "error")
        })
    })
    # DOWNLOAD HANDLER FOR  LEGEND WHICH DESCRIBES THE COLOURS IN PIE CHART 
    output$downloadLegend<-downloadHandler(
        filename = function(){
            paste0("HapNetworkLegend.jpg")
        },
        content = function(file){
            jpeg(file)
            plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
            legend("center",legend = paste0(piecols()), fill = colorz(), border = "black")
            dev.off()
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
