#' Add group ids to releases 
#' groups multiple tag codes released from the same hatchery if released within 3 weeks and 4.5 grams avg wt.
#'
#' @param release_data 
#'
#' @return
#' 
#'
group_releases <- function(release_data){

d2 <- release_data %>% 
  filter(!is.na(last_release_date)) 

# Assign release groups ####

# Unique releases
unique_rels <- unique(d2$comp_key)

# Empty vector to put release ids 
rel.id <- vector("integer", length=length(d2$comp_key))

# Progress bar 
#pb <- txtProgressBar(min=0,max=length(unique_rels),char="><((*> ", style=3, width=10)

# For each unique release group     
for (i in 1:length(unique_rels)) {
     #i <- 1
 # Subset original data for only the releases from unique release [i]
      d <- subset(d2, d2$comp_key == unique_rels[i], drop = F)    

 #  The first release for hatchery/release site/ brood year [i] is #1
      id <- 1
         # print(unique_rels[i])
 # While any tags in d have not been assigned a release id 
      while (any(rel.id[match(d$tag_code_or_release_id, d2$tag_code_or_release_id)] == 0)) {
           
       # If any avg weights for d are NA,     
           if (any(is.na(d$avg_weight))) {
                
               # Then identify which tags in d are within 21 days of the minimum date of all the tags in d,
               # the abs () is unnecessary
               g <- which(abs(d$LastRelDate - min(d$LastRelDate,na.rm=TRUE)) <= 21 ) } 
           
     # Otherwise, 
           else {
                
               # identify which tags were released within 21 days of the first release in d 
               # AND 
               # within 4.5 grams of the first release's avg.wt
               g <- which(abs(d$LastRelDate - min(d$LastRelDate,na.rm=TRUE)) <= 21 &  
                            abs(d$avg_weight - min(d$avg_weight[which.min(d$LastRelDate)])) <= 4.5)
           }
           
     # Assign the tags identifed by g (above) the current release number
       rel.id[match(d$tag_code_or_release_id, d2$tag_code_or_release_id)[g]] <- id
     
     # Subset d for the tags with no assigned release numbers   
       d <- d[ which(rel.id[ match(d$tag_code_or_release_id, d2$tag_code_or_release_id) ] == 0), ]
                         #which(d2$tag %in% d$tag)
                         
     # Bump release number up one
       id <- id + 1
          
# Loop back around to While condition
      }
      
      # When while loop for hatchery/rel site/by [i] is done, bump up progress bar 
      # and loop around to next hatchery/release site/ brood year
      
     # setTxtProgressBar(pb, i)
}
#head(d2)
# Add the release ids to the data
 d2$rel_id <-rel.id

d2
}

