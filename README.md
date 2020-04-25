# BOLDHapNet
BOLDHapNet: haplotype network creator

## INSTALLATION:
You will need to: 
### Install R or R studio
If you don’t use R often you may wish to just download R here: https://www.r-project.org

If you will use R more often you may wish to download Rstudio desktop. This has R as well as other tools such as an IDE. https://rstudio.com/products/rstudio/#rstudio-desktop 

### Install Cytoscape Desktop to visualise your networks. 
Download the most recent version of Cytoscape here: https://cytoscape.org 

### Loading the application: 
-	Open either R, or  Rstudio
-	At the console type the following: ```shiny::runGitHub(“BOLDHapNet”,“HirraF”)```
The application will load! 
If you use this application routinely you may wish to install the following packages by running the following code in the R console: 
```
list.of.packages <- c("shiny","pegas","dplyr","igraph","DT","data.table","plotly","heatmaply","randomcoloR","BiocManager","RCy3","msa")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE,quiet=TRUE)
  if(!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install(new.packages, dependencies = TRUE,quiet = TRUE)
}
```

## BASIC OVERVIEW.
BOLDHapNet allows users to create haplotype networks using the DNA Barcode data stored in BOLD Systems.  BOLDHapNet guides you through all steps of the workflow from data selection, sequence preparation and haplotype analysis. 
This application works with all operating systems. 

### FEATURES:
•	Graphical user interface, meaning no programmatic knowledge required. 
•	Interface to BOLD SYSTEMS API for data download.
•	Step by step interface reveals each step sequentially turn making the tool easy to follow. 
•	Tools for sequence preparation and alignment. 
•	Easy pie chart creation
•	Automatic generation of graph in Cytoscape
•	Help guides available for each step- simply click the steps’s ‘?’ icon. 

## STEP BY STEP WALKTHROUGH:
The below guide walks the user through an example of how this application may be used. We will explain how to interact with each step and what is going on in each step. 

## Step 1.
BOLD SYSTEMS is a database which contains information on various specimens from animal, fungi and plant kingdoms. The database holds barcode sequences from specimens alongside metadata about the specimens. This application provides and easy to use interface to the BOLD Systems handling both the download of sequence and accompanying metadata.  Users specify which datasets they would like to download directly. 
The application follows the guidance of the BOLD Systems API and allows users to enter a search string that follows the following rules:
1.	Search via the following parameters: taxon, ids, bin, container, institution, researchers and geo. Further details on each search parameter and what they refer to in the BOLD Database are available here 
2.	Format your query as follows: parameter=values.
For example, to search for records belonging to the genus Gerres
```taxon=Gerres```
Or to search for records belonging specifically to the species Gerres oyena
```taxon= Gerres oyena```
3.	Search for multiple values of a parameter by placing a ‘|’ between each value
For example, search for records belonging to the species Gerres oyena and Gerres japonica
```taxon= Gerres oyena|Gerres japonica```
4.	Multiple parameters can be combined, and they must be '&' delimited indicating logical 'AND'. 
For example, search for specimens of Gerres oyena or Gerres japonica from India:
```taxon= Gerres oyena|Gerres japonica & geo=India```

When searching for values made of multiple word e.g. 'Costa Rica', keep the space between words

#### Common mistakes to avoid:
-	Ensure you have specified each parameter you want to search e.g. specify if you want to search by taxon or bin etc. 
-	Ensure you include the ‘=’ between the parameter and values 
-	Ensure your search value is spelled correctly and exists within BOLD. You can double check by seeing if a search here://// returns any results. 

We understand that often users may want very specific datasets to analyse and the BOLD search parameters may be limiting in some way. We advise users to select as a wide a dataset as required in this step. This dataset can be further filtered and records can be removed later during Filtering in Step 4. 

## STEP 2. Selecting a barcode gene:
DNA barcoding requires certain regions of an organism’s genome to be compared. For example, animal studies often utilise a region of the cytochrome c oxidase I (COI) gene, however fungi and plants studied often use one of various genomic regions.
This step checks which barcode genes are available for use in the downloaded dataset. It presents them as choices in the format of a dropdown box. Here you can select which barcode gene you would like to work with and submit your choice. 
The table will update and remove any records that don’t use the chosen barcoding gene.

## Step 3: Greedy Search.
This feature is especially useful for users making taxon based initial searches. Greedy search looks at all the BINs in the current dataset and will return and add any specimens that are not currently in the dataset. 
BINs are Barcode Index Numbers. They represent groups of barcode sequences that have been algorithmically clustered by BOLD and closely correspond species. However species whose barcodes show significant intraspecific sequence divergence may be clustered in different BINs. Closely related species may show low interspecific sequence divergence and thus specimns of different species may get binned together. Therefore, if a search has originally been made via species, this greedy search can help present other specimens that have been found to cluster with specimens in the original table. 
For more information on BINs see here: 
http://v3.boldsystems.org/index.php/resources/handbook?chapter=2_databases.html&section=bins

### Example
Original search: ```taxon= Gerres oyena``` our table has 48 records. 
If we now select Greedy Search, we can see that our table has expanded and now contains 60 records. The species G. japonicus, G. PK-2018, G. oblongus, G. equulus and records with an unidentified species have also been added.

## STEP 4:
### Filtering the table:
Here the user can remove any unwanted records from the table to further customise their analysis. 
There are 2 different tools you can use to remove records: the ‘Simple’ tool and the ‘Advanced’ tool. 
The ‘Simple’ tool is quicker to use, but is less flexible in the filtering options you have. We suggest to try this tool first to see if it meets your needs. The ‘Advanced’ tool requires you to construct conditional expressions which define the rows you wish to remove. This may be more time consuming but, is wholly customisable. 

### Simple Filter tool:
Select the simple tool
Click ‘Select Filters’
From the pane that has appeared select column values to define the rows you wish to remove. 
Example:
In the below picture we have selected ‘No data’ in the species_name column. This means we will remove all rows that have an empty value for species_name.
- If we wanted to remove all specimens from ‘Bali’ we would select ‘Bali’ from the ‘province_state’ column.
- If we wanted to remove all specimens from Bali and Bushehr we would select both ‘Bali’ and ‘Bushehr’ from the province_state column.  
- If we selected both ‘No data’ and ‘Bali’ this would remove all unknown specimens from bali. Unknown specimens from other countries would remain. Thus this example shows how criteira specified in from different columns are combined according to logical AND behaviour.  
To remove records that have either ‘no data’ in the specimen column or are from ‘Bali’ you would need to use the Advanced filtering tool.
//IMAGE
### Advanced tool
The Advanced filtering tool allows your own custom filters. Here you can specify OR filters and filter on any table column. 
The advanced tool uses R dplyr function. The text box accepts conditional expressions (statements which can evaluate to true OR false) and the application will REMOVE any rows where the users given case is TRUE.
Your conditional expressions should define what values the rows you want to keep should or should not have.

How to define logical rules:
Any string values should be surrounded by ''. 
Examples of filter functions  you can use: (not extensive)

- Equals: == 
country=='Canada' : Selects all records from Canada 
- Not equal to: !=  
country!='Canada' : Selects all records from countries other than Canada
- For numerical values: >,  >=,  etc
recordID>40000, Selects all records with recordID's greater than 40000
- logical AND : & 
Useful for combining multiple logical rules. 
- bin_uri=='BOLD:AAB6879'&country=='Canada'
Selects records that are in the bin BOLD:AAB6879 that are Canadian
- logical OR: |
bin_uri=='BOLD:AAB6879'|country=='Canada', Selects records that are either in the bin BOLD:AAB6879 or are records from Canada
-	%in% c(): select any value in the list c() 
country %in% c('Japan','Canada'), Selects any record from Japan or Canada
- !: put in front of a logical rule to negate condition specified 
!(country %in% c('Peru','Canada')), Selects all records that are not from Peru or Canada
- is.na(): Selects any records with NA in this column, 
is.na(species_name), Selects any records with NA in the species_name column 
!is.na(species_name), Selects any records that do not have NA in the species_name column

After clicking ‘FILTER’ a new table filtered following your specified rules will be presented to you. 
You can practice and try out various filters until you are happy with the results. 
Once you are happy, click 'See table', to see your dataset with the rows removes, and then click 'CONTINUE' to confirm and set your choice. 

### Following this the application will automatically: 
1.	Retrieve sequences for your final chosen specimens. 
2.	Align the sequences using the default Clustal W algorithm.  
Sequences are then returned for the user to view or download in fasta format. 

## Viewing sequences. 
There are 3 available views of the sequences:
### Variant positions only.
This returns the alignment showing only the positions where there is more than one character present. For example if all sequences at position 3 have a ‘C’, positions 3 will not be shown. If 2 sequences at postion 5 have ‘T’ and the rest have ‘C’, positions 5 will be shown.
### Full multiple sequence alignment. 
This view shows the complete alignment
### Optional graphical view. 
NOTE: For large alignments this view can take some time to load. 
This plots the alignment as a colour coded matrix. Rows are sequences, columns are positions. Cells are coloured according to base. Hovering over rows should display the sequence name. For larger alignments this may not work without zooming in. 
You can navigate the alignment by scrolling left and right. If you scroll to the end of the alignment there are additional tools to zoom and save as a png. 

You also be presented with a table showing the base composition of each sequence. 

## Information on how haplotype assignment works:
For haplotype assignment we implement the Pegas haplotype() function. 
It uses the Hamming distance (number of nucleotides that differ between sequences) to assigned haplotypes. 
Sequences that are identical e.g. AAC, AAC have a distance of 0 so are assigned into a single haplotype. 
Sequences that differ e.g. AAC, AAT are assigned into separate haplotypes. Here the dstance between these haplotypes is 1. 

### Sequences with missing data (N characters).
Pegas assigns N characters a distance of 0. For example, AAC and AAN have a distance of 0. If an N containing sequence has a distance of 0 with exactly one other sequence, they will be assigned into one haplotype. Otherwise the N containing sequence will be assigned its own haplotype. For example, when comparing the sequences: GGGT, GGGT and GGGN, GGGT sequences form one haplotype and GGGN is a separate haplotype. The distance between these two haplotypes will be 0. 

IUPAC ambiguities are dealt with in the same way. Distances of 0 are assigned for possible match (e.g. between A and R) and distances of 1 for definite difference (e.g. between A and Y) and pooling follows the above rules.

## STEP 5: Remove any unwanted sequences.
After inspecting the alignement you may deem some sequence of bad quality and wish to remove them from the dataset.
In this step you can select to remove any sequences of bad quality. 
If you don’t wish to remove any, leave this step blank and click continue. 

## STEP 6: Trimming sequences. 
If the alignment contains sequences of different lengths you may find some sequences have trailing gaps at their start and end. 
During haplotype assignment these gaps will be treated as N characters. This can lead to sequences identical in character being separated due to length. 
Example: sequences 1: ‘TGCGCTTA’, 2: ‘TGCGCTTA’, 3: ‘TTTGCGCTTA’ and 4: ‘TGCGCT. Sequences 1, 2 and 3 will be one haplotype and the shorter sequence 4 will be a separate haplotype. 
If you wish to avoid this behaviour you may wish to trim the alignment to remove trailing gap regions. Some users may wish to remove all trailing gap regions- however this could lead to you losing variant positions which may be important for haplotype assignment. The ‘invariant trailing gap region’ output shows you how much can be removed from the start and end of the sequence without removing any variant positions. 

#### See the simplified example below:
//IMAGE

In the above alignment trailing gaps run from position 1-9 and positions 86-100. There is sequence variation at position 85. In this case the application would notify users that there are ‘Invariant trailing gap regions’ at positions 1-9 and 86-100. 
#### Cases if: 
##### No trimming 
I: A	II: B, C     III: D	 IV: E,F G 
Sequence D has been separated from E,F and G due to its length.  Sequence A has been separated from all other haplotypes as we do not know the identity of it’s nucleotide at the variant position 85.
##### Trim all sequences so no trailing gaps. 
sequence start as 10 and sequence end as 84. All sequences are of the same length but we have lost information about the variant position 85. 
All sequences would be pooled into a single haplotype.
##### Trim only invariant gap regions
set sequence starts as 10 and ends as 85. Here we prevent D , E, F and G being separated into different haplotypes due to differing lengths . 
We would be returnes these 3 haplotypes. 
I: A	II: B, C     III: D,E,F,G

To trim, select ‘Yes’ and decide what to set the sequence start position and sequence end positions as.  Otherwise click ‘No’ and continue. 

## Haplotype assignment
Following step 6 haplotype assignment automatically occurs as described previously. 
The number of haplotypes extracted are available to view in the main panel. 

The haplotype network is also computed. 
Haplotypes are joined according to the TCS statistical parsimony algorithm (). Here the algorithm estimates a limit or parsimony- a maximum distance at which haplotypes separated by more than this distance will not be joined. TCS then sequentially links haplotypes below this limit, starting with haplotype pairs separated by a distance of 1, then 2 and so on.
The original paper for this algorithm can be found here
A simplified explanation of the algorithm can be found here. 

## STEP 7: Alternative links
The algorithm may also find alternative ways to links the nodes. These are called alternative links. You can select to view the network with the alternative links. 

The main panel will show a summary of the network, detailing the number of haplotypes. Links refer to the number of edges in the graph and link lengths are the distances between the haplotypes. 


## STEP 8: PIE CHARTS 
Pie charts are often added to the nodes (which represent haplotypes) to show a certain feature of the specimens involved in the analysis. For example, users may wish to colour nodes by country of the specimen’s origin. Haplotypes containing specimens from multiple countries will be coloured using a pie chart, coloured in proportion to the frequency of each country.  This would allow you to investigate the geographic distribution of haplotypes with ease. 
Using the drop down box select which column of data you would like to use to plot pie charts with. 

## STEP 9: Select an edge type
Edges of a haplotype network link closely related haplotypes and they are labelled with the number of nucleotide differences between linked haplotypes. 
Our application gives the option of 2 edge decorations to signify the step either as ‘dot’s which or as simple numeric edge labels. 
The default option is to present steps as dots. However we recommend selecting numeric labels for larger networks that may have many steps between haplotypes. 

## STEP 10: GENERATING A CYTOSCAPE GRAPH
This application constructs an output in the desktop application Cytoscape. Cytoscape is a popular desktop software used for plotting network. It offers users many customisable options and has a simple point and click interface. 
At this step ensure Cytoscape is open and your graph will be outputted there. 
There is also an option to Download a corresponding PNG legend which explains pie chart colours. 

## FURTHER CUSTOMISATION IN CYTOSCAPE.
Cytoscape was chosen the graphing software for this application due to its many options for customisation. 
Nodes can be moved simply by clicking and dragging. 
The space between nodes can be increased and decreased using the Node Layout Tools option. 
Click Layout > Node Layout tools
Here by adjusting the scale the space between nodes, size of network and rotation of network can be adjusted. 

Text and images, such as the Legend downloaded from the application can be added to the network image via the Annotation options. 

Many more customisation options are available see Cytoscape manuals for all features. 




