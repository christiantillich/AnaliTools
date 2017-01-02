#' Title s3read
#' @description A windows-based s3read command. Designed to mimic the function
#' of robertzk/s3mpi, but using windows system commands.
#' @param bucket The top-level bucket the data is stored in.
#' @param filepath The remaining path to the file
#' @return Returns the serialized R object.
#' @export
s3read <- function (bucket, filepath){
  #Check if your creds exist

  #Create a blank tempfile and directory for storage.
  x.serialized <- tempfile()
  dir.create(
     dirname(x.serialized)
    ,showWarnings = FALSE
    ,recursive = TRUE
  )

  #If this function fails or if the temp file already exists, free up the file.
  on.exit(unlink(x.serialized), add = TRUE)
  if (file.exists(x.serialized)) {
    unlink(x.serialized, force = TRUE)
  }

  #The windows-based amazon CLI command to read in the file.
  s3.cmd <- paste(
    "aws s3api get-object"
    ,"--bucket", bucket
    ,"--key", filepath
    , x.serialized
  )
  dummy <- system(s3.cmd)
  readRDS(x.serialized)
}

#' Title s3write
#' @description A windows-based s3write command. Designed to mimic the function
#' of robertzk/s3mpi, but using windows system commands.
#' @param bucket The top-level bucket the data is stored in.
#' @param filepath The full path where you'll store the file
#' @return NULL
#' @export
s3write <- function(obj, bucket, filepath){

  #Create a blank tempfile and directory for storage.
  x.serialized <- tempfile()
  dir.create(
    dirname(x.serialized)
    ,showWarnings = FALSE
    ,recursive = TRUE
  )

  #If this function fails or if the temp file already exists, free up the file.
  on.exit(unlink(x.serialized), add = TRUE)
  if (file.exists(x.serialized)) {
    unlink(x.serialized, force = TRUE)
  }

  #Write out the R object as a serialized object
  saveRDS(obj, x.serialized)

  #The windows-based amazon CLI command to write out an object.
  s3.cmd <- paste(
    "aws s3api put-object"
    ,"--body", x.serialized
    ,"--bucket", bucket
    ,"--key", filepath
  )
  dummy <- system(s3.cmd)
}

#' Title s3bucketlist
#' @description Calls all of the available s3 buckets.
#' @return Returns a data frame listing the available s3 buckets and some
#' attributes
#' @export
s3bucketlist <- function(){
  out <- system('aws s3api list-buckets', intern=T) %>%
    paste(collapse='') %>%
    jsonlite::fromJSON(txt=.)

  out["Buckets"]
}


#' s3list
#' @param bucket The bucket we'll be searching
#' @param prefix Optional to restrict your search to a specific directory path.
#' @param lim Optional
#'
#' @return
#' @export
#'
#' @examples
s3list <- function(bucket, prefix="", lim=if(prefix==""){20}else{10000}){

  if(prefix == ""){
    s3.cmd <- paste(
      "aws s3api list-objects"
      ,"--bucket", bucket
      ,"--max-items", lim
    )
  }else{
    s3.cmd <- paste(
      "aws s3api list-objects"
      ,"--bucket", bucket
      ,"--prefix", prefix
      ,"--max-items",lim
    )
  }

  out <- system(s3.cmd,intern=T) %>% jsonlite::fromJSON(txt=.)

  out$Contents %>%
    mutate(
       LastModified = as.Date(LastModified)
      ,ETag = substr(ETag, nchar(ETag)-7, nchar(ETag)-1) %>% paste0('...',.)
      ,Size = paste(round(Size / 100000,0), 'Mb')
    ) %>% as.data.frame
}

