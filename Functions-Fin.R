#list of packages required
list.of.packages <- c("shiny","pegas","dplyr","igraph","DT","data.table","plotly","heatmaply","randomcoloR","BiocManager","RCy3","msa")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE,quiet=TRUE)
  if(!requireNamespace("BiocManager", quietly = TRUE))
    BiocManager::install(new.packages, dependencies = TRUE,quiet = TRUE)
}

suppressPackageStartupMessages({
library(pegas)
library(msa)
library(dplyr)
library(igraph)
library(RCy3)
library(DT)
library(data.table)
library(plotly)
library(heatmaply)
library(randomcoloR)
})

base_url_spec<-'http://www.boldsystems.org/index.php/API_Public/specimen?'
base_url_seq<-'http://www.boldsystems.org/index.php/API_Public/sequence?'


#' download the speciman table from BOLD Systems
#'@section Details.
#'The Bold System Database can be searched via certain services including: 
#'taxon, ids, bin, container, institution, researchers and geo. 
#'A search can be made using multiple services which must be '&' delimited, combining the 
#'services with a logical "AND" clause. 
#'Each service can take multiple parameters, which must be "|" delimited, combining parameters
#'with a logical "OR" clause. 
#'Any parameter made of multiple words should be passed in retaining their spaces e.g "Costa rica"
#'
#' @param search_string a string specifying the specimens to be returned
#'
#' @return Returns data table containing speciman information.
#' @export 
#' @examples  
#' recordTableCreator("bin=BOLD:AA2357|BOLD:AA56777")- will return specimans from bins BOLD:AA2357 or BOLD:AA56777
#' recordTableCreator("bin=BOLD:AA2357|BOLD:AA56777&geo=Costa rica ) will return only specimans from bins BOLD:AA2357 or BOLD:AA56777 which 
#' are from Costa rica. 
#' @seealso  Further details on each search parameter are available here http://boldsystems.org/index.php/resources/api?type=webservices
recordTableCreator<-function(search_string){
  full_url_spec<- paste0(base_url_spec,search_string,'&format=tsv')
  full_url_spec<-URLencode(full_url_spec)
  full_url_spec1<-gsub("%20&%20","&",full_url_spec, fixed = TRUE)
  spec_tsv<-read.csv(full_url_spec1, sep = '\t', header = TRUE, na.strings = '')
  return(spec_tsv)
}

#' Search and download sequences from Bold Systems
#'@section Details 
#'The Bold System Database can be searched via certain services including: 
#'taxon, ids, bin, container, institution, researchers and geo. 
#'A search can be made using multiple services which must be '&' delimited, combining the 
#'services with a logical "AND" clause. 
#'Each service can take multiple parameters, which must be "|" delimited, combining parameters
#'with a logical "OR" clause. 
#'Any parameter made of multiple words should be passed in retaining their spaces e.g "Costa rica"
#'
#' @param search_string a string specifying the sequences to be returned
#'
#' @return DNA String set containing sequences
#' @export
#'
#' @examples
#' dnaSSCreator("bin=BOLD:AA2357|BOLD:AA56777")- will return sequences from bins BOLD:AA2357 or BOLD:AA56777
#' dnaSSCreator("bin=BOLD:AA2357|BOLD:AA56777&geo=Costa rica )
#'  will return only sequences from bins BOLD:AA2357 or BOLD:AA56777 which are from Costa rica. 
#' 
dnaSSCreator<-function(search_string){
  full_url_seq<- paste0(base_url_seq,search_string)
  full_url_seq<-URLencode(full_url_seq)
  full_url_seq<-gsub("%20&%20","&",full_url_seq, fixed = TRUE)
  seqs<-read.dna(full_url_seq,format="fasta")
  #seqs<- readDNAStringSet(full_url_seq)
  #delete any gaps- trailing and internal- present in each sequence
  nogap <- del.gaps(seqs)
  ss <- nogap %>% as.character %>% lapply(., paste0, collapse = "") %>% unlist %>% DNAStringSet
  return(ss)
}

#' Greedy Search.
#' @section Details  
#' Look at the bins in current table. Will return add additional sequences from these bins
#' that are not currently in the dataset/ 
#'
#' @param current_table The current table downloaded from Bold Systems.  
#'
#' @return list of URLS: 
#' * [1] url for downloading specimen table for the described bins. 
#' * [2] url for downloading the sequences for described bins 
#' @export
#'
#' @examples
#' table1<- recordTableCreator("taxon=Aves" )
#' greedySearchUrls(table1)
#' 
#
greedySearchUrls<-function(table_1){
  all_bins<-levels(table_1$bin_uri)
  all_bins<-all_bins[all_bins!=""]
  new_url_spec<-paste0(base_url_spec,'format=tsv','&bin=')
  new_url_seq<-paste0(base_url_seq)
  for(i in 1:length(all_bins)){
    if(i==1){
      new_url_spec<-paste0(new_url_spec,all_bins[i])
      new_url_seq<-paste0(new_url_seq,'bin=',all_bins[i])
    } else{
      new_url_spec<-paste0(new_url_spec,'|',all_bins[i])
      new_url_seq<-paste0(new_url_seq,'|',all_bins[i])
    }
  }
  new_url_spec<-URLencode(new_url_spec)
  new_url_seq<-URLencode(new_url_seq)
  return(list(new_url_spec, new_url_seq))
}

#' Show which barcoding genes are present in the dataset. 
#'@section Details 
#' Various different types of barcoding genes are available e.g.(COI-5P or atp6) and for a given specimen 
#' BOLD systems will return all genetic barcodes it has in its database. To build haplotype
#' network, the same gene must be compared. This function shows what genetic barcodes are 
#' available to analyse in the dataset selected, allowing the user to choose.  
#' 
#' @param DNA_SS DNA String Set obtained from BOLD
#'
#' @return table listing gene markers in the String Set and their frequency. 
#' @export
#'
#' @examples
#' ss<-dnaSSCreator("taxon=Bos javanicus")
#' geneMarkerAvailable(ss)
#' 
#' @seealso geneMarkerSelection()
#' 
geneMarkerAvailable<-function(DNA_SS) {
  idsinseq<-lapply(names(DNA_SS), function(x) unlist(strsplit(x, '|',fixed=TRUE)))
  genemarker<-sapply(idsinseq, "[[",3)
  tab_gm<-table(genemarker)
  return(tab_gm)
}

#' Selecting a barcoding gene to analyse.
#' @section Details 
#' User may select following analysis of the barcoding gene types available in the dataset 
#' using the geneMarkerAvailable() function. 
#' 
#' @param DNA_SS DNA String Set containing sequences to select from
#' @param selected_gene The name (as String) of barcoding gene user has selected to analyse.
#' 
#'
#' @return DNA String set containg only sequences that are from the selected barcoding gene
#' @export
#'
#' @examples
#' geneMarkerSelection(DNA_SS,"COI-5P")
#' @seealso geneMarkerAvailable()
geneMarkerSelection<-function(DNA_SS, selected_gene){
  return_seqs<-DNAStringSet()
  for(i in 1:length(names(DNA_SS))){
    sp<-as.list(strsplit(names(DNA_SS)[i],'|',fixed=TRUE)[[1]])
    if(sp[3]== selected_gene){
      return_seqs<-c(return_seqs,DNA_SS[i])
    }
  }
  return(return_seqs)
}

#' Remove from table all records that do not have a sequence
#'
#' @param spec_table speciman record table from BOLD
#' @param DNA_SS sequence string set 
#'
#' @return table containing only records of specimen with sequence in dataset. 
#' @export
#' 
#' @examples
#' search_string<-"taxon=Bos javanicus"
#' bold_table<-recordTableCreator(search_string)
#' bold_seqs<-dnaSSCreator(search_string)
#' cleanTable(bold_table, bold_seqs)
#' 
cleanTable<-function(spec_table, DNA_SS){

    idintable<- as.character(spec_table$processid)
    idinseq1<-lapply(names(DNA_SS), function(x) unlist(strsplit(x, '|',fixed=TRUE)))
    idinseq1<-sapply(idinseq1, "[[",1)
    #get ids that are in table and in seqs 
    in_both<-idintable[(idintable%in%idinseq1)]
    #filter out rows for which we have no sequence
    clean_tsv<-filter(spec_table,spec_table$processid%in%in_both)
    return(clean_tsv)
  
}


#' Filter specimen table according to user preference
#'
#' @param table_1 current specimen table
#' @param filter_statement_str string describing logical rules to filter table on. 
#'
#' @return filtered table containing only records wanted by user/ 
#' 
#' @section Details:
#' filter_statement_str is a string declaring the logical rules that 
#' the table should be filtered on. Function will return rows where the user's given cases 
#' are TRUE. 
#' User should define logical rules in terms of the variables (columns) in the data table 
#' and filter statements that should define values the column should match in order for row
#' to be selected. 
#' logical rules should follow the format of dplyr::filter() function i.e:
#' colname (some filter function) x
#' Examples of filter function: (not extensive)
#' * ==, >, >= etc
#' * &, |, !
#' * is.na()
#' * %in% c() -  logical rule to state colname is equivalent to one of the values in a list (c())
#' 
#' Any parameters in the filter_statement_str that need to be quoted (such as string values)
#' must be passed using ''. 
#' 
#' @export
#'
#' @examples
#'  * selects rows where institution_stroing is Manchester or MMU
#' filter_statement_str<-"institution_storing %in% c('Manchester', 'MMMU' )"
#' filterTable(table_1,filter_statement_str)
#' 
#' * selects rows where either country is not Canada or the collector is John Green 
#' filter_statement_str<-"country!='Canada'|collectors=='John Green'"
#' filterTable(table_1,filter_statement_str)
#' 
filterTable<-function(table_1,filter_statement_str){
  if(filter_statement_str !=""){
    filter_condition<-rlang::parse_expr(filter_statement_str)
    sorttbl<-table_1%>%
      filter(!!filter_condition)
  } else {
    sorttbl<-table_1
  }
  return(sorttbl)
}


#following table filtering, select the final sequence.
#' Select sequences to match a filtered table
#'
#' @param filtered_table table containg desired records
#' @param DNA_SS DNA String Set to filter and select from.
#'
#' @return String Set containing sequences which match the filtered table. 
#' 
#' @export
#'
#' @examples
#' filter_statement_str<-"country!='Canada'|collectors=='John Green'"
#' filtered_table<-filterTable(table_1,filter_statement_str)
#' selectSeqs(filtered_table, DNA_SS)
#' 
#' @seealso filterTable()
#' 
selectSeqs<-function(filtered_table,DNA_SS){
  idtokeep<-filtered_table$processid
  finalSS<-DNAStringSet()
  for (i in 1:length(DNA_SS)){
    sp<-unlist(strsplit(names(DNA_SS)[i],'|',fixed=TRUE))
    if(is.element(sp[1],idtokeep)){
      finalSS<-c(finalSS,DNA_SS[i])
    }
  }
  return(finalSS)
}

#' Remove end gaps from a seqeunce. 
#'@section Details:
#'end gap refers to trailing ----- or NNNN that can be 
#'found at the beginning and end of sequences
#' @param DNA_SS DNA String Set 
#'
#' @return DNA String set, with each sequences end gaps removed
#' @export
#'
#' @examples
#' bold_seqs<-dnaSSCreator(search_string)
#' removeEndGaps(bold_seqs)
removeEndGaps<-function(DNA_SS){
  cleanSS_list<-lapply(DNA_SS, function(x) gsub(pattern ='^[^ATCG]+',"",x))# remove leading
  cleanSS_list<-lapply(cleanSS_list, function(x) gsub(pattern ='[^ATCG]+$',"",x)) # remove trailing
  cleanDS_list<-sapply(cleanSS_list, function(x) DNAString(x))
  clean_DSS<-DNAStringSet(cleanDS_list, use.names=TRUE)
  return(clean_DSS)
}


##removes position where Ns in non mismatch positions 
##and also removes trailing - positions if there are no mismatches. 

#' Removing N from sequence
#'
#' @param SS aligned DNA string set 
#'
#' @return DNA String set with certain non-ATCG psoitions removed- see details
#' @export
#'@details 
#'This function finds positions in an aligned DNA String Set where non-ATCG characters are 
#'present. It removes this position if there are no mismatches at this position. 
#'A mismatch is defined here as an ATCG charcter aligned with a different ATCG character 
#'- no informative variation at this position,  
#' @examples
#' N gets removed in this alignment, No ATCG mismatch at position 5
#' AAAANTTTCG
#' AAAAATTTCG
#' AAAAATTTCG
#' 
#' N doesn't get removed ATCG mismatch at  position 4.
#' AAANTTTGG
#' AAAGTTTGG
#' AAACTTTGG


removenonvar<- function(SS){
  mat <- as.matrix(SS)
  tbl_mat <- as.data.frame(mat) 
  tbl_var<- tbl_mat %>% select_if(~length(unique(.)) > 1)
  final_mat<-as.matrix(tbl_var,rownames=TRUE)
  tbl_SS<-as.alignment(final_mat)
  return(tbl_SS)
}




#' Return the gap positions at the ends of the sequence
#'
#' @param SS DNA String Set
#'
#' @return return the 
#' @export
#'
#' @examples
gappos<-function(SS){
  seqs_returned<-SS
    npos<-c()
    #for each seq capture positions where non atcg char present.
    for(s in 1:length(seqs_returned)){
      n1<-gregexpr("[-]",seqs_returned[s])
      npos<-c(npos,n1)
    }
    npos<-unique(unlist(npos))
    nrem<-c()
    #check these pos in the alignment. 
    for(n in npos){
      x<-substring(seqs_returned,n,n)
      `attributes<-`(x, NULL)
      z<-table(x)
      #ensure this position contain only N and one other char. 
      if(length(z)==2){
        nrem<-c(nrem,n)
      }
    }
    nremoval<-unique(nrem)
return(nremoval)
}


#trim sequences at a certain position. 
#
#' Trim DNA Sequence alignemnt.
#' User specifies the positions they wish their DNA Sequence Alignment to 
#' start and end at. 
#'
#' @param aligned_SS MSA
#' @param pos_to_start Desired start position
#' @param pos_to_end Desired end position
#'
#' @return trimmed MSA.
#' @export
#'
#' @examples
#' trimSeqs(seqs,1,575)
trimSeqs<-function(aligned_SS, pos_to_start, pos_to_end){
  trimm_SS<-lapply(aligned_SS, function(x) paste0(substr(x, pos_to_start, pos_to_end)))
  trimm_SS<-sapply(trimm_SS, function(x) DNAString(x))
  trimm_SS<-DNAStringSet(trimm_SS, use.names=TRUE) 
  return(trimm_SS)
}



#' Make Haplotype from DNA STRING SET
#'
#' @param SS DNA STRING SET 
#'
#' @return Haplotype
#' @export
#'
#' @examples
makeHaploType<-function(SS){
  seq_Hap<- as.DNAbin(SS) %>%
    haplotype()
  return(seq_Hap)
}

#makes the haplonet- defines nodes and edges 
makeHaploNet<-function(haploType){
  seq_Net<-haploNet(haploType)
  return(seq_Net)
}


#make the frequency matrix. The default factor counted is Country
#' Creates matrix telling you the value a seqeunce's metadata has from chosen column
#' help with creating pie charts. 
#' @param SS DNA String Set
#' @param record_table Metadata in table
#' @param freq_ind The column number from table chosen to obtain value from
#' Default is freq_ind=55 which corresponds to country data.
#'
#' @return matrix: seq_name: value
#' @export
#'
#' @examples
#' freq_Matrix(seqs, bold_table)
#' freq_Matrix(seqs, bold_table,22)
freq_Matrix<-function(SS,record_table, freq_ind=55){
  freq_m<-matrix(nrow=length(names(SS)),ncol=1)
  splitrow<-as.list(strsplit(names(SS),"|",fixed=TRUE))
  freqmnam<-c()
  for(x in 1:length(splitrow)){
    freqmnam<-c(freqmnam,splitrow[[x]][1])
  }
  rownames(freq_m)<-freqmnam
  #now fill the frequency matrix 
  for(i in 1:length(rownames(freq_m))){ #for each sequence
    for(y in 1:nrow(record_table)){ #for each row
      if(rownames(freq_m)[i]== record_table$processid[y]){ #if looking at same record
        if(is.na(record_table[y,freq_ind])== TRUE){
          freq_m[i]<-as.character("N..A")
        } else{
          freq_m[i]<-as.character(record_table[y,freq_ind])#place the value of its parameter against the name.
        }
      }
    }
  }
  return(freq_m)
}

#creates the table with rows= haplotype and column is factor:how many 
#specimens in haplotype are of certain level. 
#' Creates table used for pie charts. 
#' Links data from freq_matrix to haplotype assignment 
#' Each row is haplotype. columns are possible values e.g. countries
#' Tallys how many sequences in a haplotype are a certain value. 
#'
#' @param freq_MMatrix output from freq_Matrix()
#' @param haploType haplotype
#'
#' @return matrix giving number of sequences in a haplotype are of certain value.
#' @export
#'
#' @examples
#' f <- freq_matrix(seqs, bold_table)
#' freq_hap_table(f,haplotype)
freq_Hap_table<-function(freq_MMatrix,haploType){
  factors<-unique(freq_MMatrix[,1])
  freqhap<- matrix(0,nrow=length(attr(haploType,"index")),ncol=length(factors))
  colnames(freqhap)<-factors
  for(lev in colnames(freqhap)){ #for each factor
    for(x in 1:nrow(freqhap)){ #go down each haplotype
      for (y in 1:length(freq_MMatrix)){#go down each sequnce
        if(is.element(y,attr(haploType,"index")[x][[1]])){ #if the seq index is in the haplotype
          if(freq_MMatrix[y]== lev){
            freqhap[x,lev]<-freqhap[x,lev]+1
          }
        }
      }
    }
  }
  return(freqhap)
}


#add attributes to the graph table: one for each factor level. - table columns.
#also sets the final size of the nodes on the graph. 
#' Adds the pie chart attribute to a igraph 
#' each column in inputted table/matrix = list of numbers which represent how many sequences/ haplotype have the column's value
#' Function also sets the size of the nodes as 30*sqrt(number of seqs in hap)
#'
#' @param freqhaptable matrix tallying nuber of seqs/ haplotype that have a certain value. 
#' @param i_graph igraph object
#'
#' @return igrpah object with additional attributes specifiying pie chart columns and size.
#' @export
#'
#' @examples
#' f <- freq_matrix(seqs, bold_table)
#' g <- freq_hap_table(f,haplotype)
#' pie_Cols_add<- (g, igraph)
pie_Cols_add<-function(freqhaptable, i_graph){
  piecols<-c()
  for(n in row.names(freqhaptable)){
    i_graph<-set_vertex_attr(i_graph,"size",index=V(i_graph)[name=n], value=30*sqrt(sum(freqhaptable[n,])))
    for(i in colnames(freqhaptable)){
      lbl<-paste0(gsub("[[:space:]]|[[:punct:]]", "_",i))
      i_graph<-set_vertex_attr(i_graph,lbl,index=V(i_graph)[name=n],value=freqhaptable[n,i])
      piecols<-c(piecols,lbl)
    }
  }
  return(i_graph)
}

#add other attributes to the igraph 
#' Specify further attributes to the igraph such as:
#' what final labels of haplotype nodes will be. 
#' what edge steps/ distance between haplotypes are.
#'
#' @param i_graph igraph object to add attribute to
#' @param hapNet haplonet where steps information will be obtained from
#'
#' @return igrpah with additional specified attributes
#' @export
#'
#' @examples
#' net<-makeHaplonet(haplotype)
#' igraphProperties(igraph, net)
igraphProperties<-function(i_graph,hapNet){
  #set the final labels for major nodes 
  V(i_graph)$finallabel<-(attr(V(i_graph),'names'))
  #set edge attribute as number of steps
  E(i_graph)$steps<-as.integer(hapNet[,"step"])
  return(i_graph)
}

# same for alterntaive link grpahs. 
igraphPropaltlink<-function(i_graph,hapNet){
  #set the final labels for major nodes 
  V(i_graph)$finallabel<-(attr(V(i_graph),'names'))
  #set edge attribute as number of steps
  E(i_graph)$steps<-as.integer(c(hapNet[,"step"],
                                 attr(hapNet,"alter.links")[,"step"]))
  return(i_graph)
}

#' Fucntion which creates subnodes to represent the steps between haplotypes
#' e.g: 2 haplotypes separated by 3 polymorphisms will have 3 subnodes between them.
#'
#' @param original_edges Original edge names. extract from igraph using: 
#' as.list(attr(E(igraph), "vnames"))
#' @param i_graph the igraph subnodes to be added to.
#'
#' @return igraph object with subnodes added between haplotype nodes.
#' @export
#'
#' @examples
#' o <- as.list(attr(E(my_igraph), "vnames"))
#' createSubnodes(o, my_igraph)
#' 
createSubNodes<-function(original_edges,i_graph){
  newnodes<-c()
  for(e in 1: length(E(i_graph))){
    x<-1
    listsubnode<-c()
    while(x<=E(i_graph)$steps[e]){
      nn<-paste0((attr(E(i_graph), "vnames")[e]),'.',as.character.default(x))
      i_graph<-i_graph+vertex(nn,size=10,color='#808080',finallabel=' ')
      listsubnode<-c(listsubnode,nn)
      newnodes<-c(newnodes, nn)
      x<-x+1
    }
    k<-1
    node1<-(ends(i_graph,(attr(E(i_graph), "vnames")[e]), names = TRUE)[1,1])
    i_graph <- i_graph + edge(node1, listsubnode[k],weight=0.1)
    while(k<length(listsubnode)){
      i_graph <- i_graph + edge(listsubnode[k], listsubnode[k+1],weight=0.1)
      k<-k+1
    }
    node2<-(ends(i_graph,(attr(E(i_graph), "vnames")[e]), names = TRUE)[1,2])
    i_graph <- i_graph + edge(node2, listsubnode[k],weight=0.1)
  }
  # #remove the old edges
  for(x in original_edges){
    i_graph <- i_graph - edge(x)
  }
  return(i_graph)
}



# Return to user which sequence is in which haplotype
nameinHaplotype <- function(haplo, seqs) {
  haplo_list <- attr(haplo,"index")
  output_table <- matrix(0,nrow=length(haplo_list))
  for(i in 1:length(haplo_list)){
    list_numb <- haplo_list[[i]]
    namesplit <- sapply(names(seqs)[list_numb], function(x) strsplit(x,'|',fixed=TRUE))
    processid <- lapply(namesplit, "[[",1)
    output_table[i,1]<-toString(processid)
  }
  dimnames(output_table)<-list(dimnames(haplo)[[1]],c('Sequence id'))
  return(output_table)
}

# using process IDs, remove sequences from the final dataset
removeSeqs <- function(idtoremove, SS){
  finalseqs <- DNAStringSet()
  for (x in 1:length(SS)) {
    sp<-as.list(strsplit(names(SS)[x],'|',fixed=TRUE)[[1]])
    if (!is.element(sp[1],idtoremove)) {
      finalseqs<-c(finalseqs,SS[x])
    }
  }
  return(finalseqs)
}


printMultipleAlignment <- function(alignment, chunksize=60)
{
  # this function requires the Biostrings package
  require("Biostrings")
  # find the number of sequences in the alignment
  numseqs <- alignment$nb
  # find the length of the alignment
  alignmentlen <- nchar(alignment$seq[[1]])
  starts <- seq(1, alignmentlen, by=chunksize)
  n <- length(starts)
  # get the alignment for each of the sequences:
  aln <- vector()
  lettersprinted <- vector()
  for (j in 1:numseqs)
  {
    alignmentj <- alignment$seq[[j]]
    aln[j] <- alignmentj
    lettersprinted[j] <- 0
  }
  # print out the alignment in blocks of 'chunksize' columns:
  for (i in 1:n) { # for each of n chunks
    for (j in 1:numseqs)
    {
      alnj <- aln[j]
      chunkseqjaln <- substring(alnj, starts[i], starts[i]+chunksize-1)
      chunkseqjaln <- toupper(chunkseqjaln)
      # Find out how many gaps there are in chunkseqjaln:
      gapsj <- countPattern("-",chunkseqjaln) # countPattern() is from Biostrings package
      # Calculate how many residues of the first sequence we have printed so far in the alignment:
      lettersprinted[j] <- lettersprinted[j] + chunksize - gapsj
      print(paste(chunkseqjaln,lettersprinted[j],alignment$nam[j]))
    }
    print(paste(' '))
  }
}


# return table showing the base composition of each sequence.
baseComposition <- function(SS) {
  listtables<-list()
  for(i in 1:length(SS)){
    y1<-as.vector(SS[[i]])
    x1<-table(y1)
    x2<-t(as.matrix(x1))
    listtables[[i]]<- x2
  }
  finmat<-do.call(rbind,listtables)
  finmat2<-finmat[, colSums(finmat != 0) > 0]
  `row.names<-`(finmat2, names(SS))
}


