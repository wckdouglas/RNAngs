#' groupGeneType
#'
#' This function grouped ensembl gene types to bigger gene group type
#'
#' @param x1: \code{geneType}
#'
#' @return y1: new group name
#'
#' @keywords keywords
#'
#'
ncRNA=c("sense_intronic","3prime_overlapping_ncrna",'processed_transcript',
    'sense_overlapping','Other_lncRNA')
smncRNA=c('misc_RNA','snRNA','piRNA')
large_rRNA=c('28S_rRNA','18S_rRNA')
small_rRNA=c('rRNA','5S_rRNA','58S_rRNA','5.8S_rRNA')
protein_coding = c('protein_coding','TR','IG')

#' @export
groupGeneType <- function(x){
    ifelse (x %in% ncRNA, 'Other ncRNA',
           ifelse (grepl('TR',x), 'TR',
                  ifelse (grepl('IG',x), 'IG',
                         ifelse (grepl('Mt_',x), 'Mt',
                                 ifelse (grepl('tRNA',x), 'tRNA',
                                         ifelse (x %in% small_rRNA, '5/5.8S rRNA',
                                                 ifelse (x %in% large_rRNA, '18/28S rRNA',
                                                         ifelse (x %in% smncRNA, 'Other sncRNA',
                                                                 ifelse (grepl('pseudogene',x), 'Pseudogenes', x
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         )
                                 )
                         )
                  )
           )
}
