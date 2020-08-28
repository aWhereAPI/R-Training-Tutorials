filesToProcess <- list.files(path = 'archives/master_version/')
languagesToProcess <- c('english','spanish')

for (x in 1:length(filesToProcess)) {
  fileToUse <- filesToProcess[x]
  
  for(z in 1:length(languagesToProcess)) {
    
    languageToUse <- languagesToProcess[z]
    
    outputFile <- paste0('current_training_scripts/',languageToUse,'/',gsub(pattern = '_master.txt'
                                                                            ,replacement = '.R'
                                                                            ,x = fileToUse
                                                                            ,fixed = TRUE))
    
    if (file.exists(outputFile)) {
      unlink(outputFile)
    }
    
    
    
    dir.create(path = paste0('current_training_scripts/',languageToUse,'/')
               ,showWarnings = FALSE
               ,recursive = TRUE)
    
    
    
    con <- file(paste0('archives/master_version/',fileToUse),'r')
    
    keepReading <- TRUE
    commentBlock <- FALSE
    skipComment <- FALSE
    
    lineCounter <- 0
    
    while (keepReading == TRUE) {
      lineCounter <- lineCounter + 1 
      
      keepLine <- TRUE
      line = readLines(con, n = 1)
      if (length(line) == 0) {
        #this occurs on the last line which should be blank
        keepReading <- FALSE
      } else if (grepl(pattern = 'if (language =='
                       ,x = line
                       ,fixed = TRUE)) {
        
        commentBlock <- TRUE
        keepLine <- FALSE
        
        #if the language of the block matches what should be exported
        if (grepl(pattern = languageToUse
                  ,x = line)) {
          
          skipComment <- FALSE
          
          #if it doesn't
        } else {
          
          skipComment <- TRUE
        }
        
      } else if (commentBlock == TRUE & grepl(pattern = '}'
                                              ,x = line)) {
        commentBlock <- FALSE
        keepLine <- FALSE
        skipComment <- FALSE
        
      } else if (skipComment == TRUE) {
        keepLine <- FALSE
      }
      
      #remove leading white space on these comments
      if(commentBlock == TRUE & keepLine == TRUE) {
        if (grepl(pattern = '#'
                  ,x = line)) {
          
        #  line <- trimws(line,which = 'left')
          line <- substr(line,2,nchar(line))  
        } else {
          stop('Comment Block not formatted properly\n')
        }
        
      }
      
      if (keepLine == TRUE) {
        write(line,file = outputFile,append = TRUE)
      }
      
    }
    
    close(con)
  }
}


