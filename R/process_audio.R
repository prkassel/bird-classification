library(seewave)
library(tuneR)
library(caret)
library(tidyverse)
library(abind)
library(parallel)


df <- read_csv('./output/data.csv') %>% as.data.frame()
df <- df %>% filter(substring(ebird_code, 1, 1) %in% letters[1:13])

top_5 <- df %>% group_by(ebird_code) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:5) %>% select(ebird_code)

dat <- data.frame()
#df <- df %>% filter(species %in% top_5$species)


dir <- paste(getwd(), 'data/xeno-canto-bird-recordings-extended-a-m/A-M/comrav', sep="/")
files <- list.files(path=dir)
files <- files[1:10]


gen_file_specs <- function(filename, size=10) {
  file <- readMP3(paste(dir, filename, sep="/"))
  file <- downsample(file, 22050)
  duration <- length(file@left)/file@samp.rate
  remainder <-  duration %% (size / 2)
  
  if (duration < size) {
    stop <- duration - remainder
  }
  else {
    stop <- duration - size - remainder
  }
  windows  <- seq(0, stop, size / 2)
  print(windows)
  process_sample <- function(start_point) {
    if (duration < size) {
      to <- duration
    }
    else {
      to <- start_point + size
    }
    clip <- cutw(file, f=file@samp.rate, from=start_point, to=to, output = "Wave")
    
    ### Generate spectrogram  (begins as 256 X 256)
    m <- melfcc(clip, sr=clip@samp.rate, usecmp=TRUE, minfreq = 1400,
      spec_out = TRUE, nbands=256, hoptime=size/256)$aspectrum

   ### Stride through the spectrogram matrix at 64X64 "pixel box" intervals
   ### and find the one that has the greatest standard deviation
    boxes <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                      c("h","h_end","v","v_end","sta"))
    print(dim(m))
    
    s <- seq(1,208,16)
    for (v in s) {
      # v_end <- v + 63
      # for (h in s) {
      #   h_end <- h + 63
      #   #print(c(h,h_end,v,v_end))
      #   
      #   box <- try(m[h:h_end,v:v_end])
      #   if(inherits(box, "try-error")) {
      #     next
      #   }
      #   boxes <- boxes %>% add_row(h,h_end,v,v_end,sta=sd(box))
      # }
      v_end <- v + 63
      box <- try(m[v:v_end,1:256])
      if(inherits(box, "try-error")) {
        next
      }
      boxes <- boxes %>% add_row(h=v,h_end=v_end,v=1,v_end=256,sta=sd(box))
    }
    
    ### box with the greatest standard dev
    b <- boxes[which.max(boxes$sta),]
    if (nrow(boxes) > 0) {
      return(m[b$h:b$h_end,b$v:b$v_end])
    }
    else {
      return(NULL)
    }
  }

  samples <- mclapply(windows, function(window) {
    process_sample(window)
  }, mc.cores = 4)
  # samples <- lapply(windows, function(window) {
  #   process_sample(window)
  # })

  samples
}


process_files <- function() {
  dat <- data.frame(filenames=c(), spectrograms=c())
  
  for (file in files) {
    print(file)
    spectrograms <- gen_file_specs(file)
    filename <- c()
    for (spec in spectrograms) {
      filename <- c(filename, file)
    }
    labeled_spectrograms <- cbind(filename, spectrograms)
    dat <- rbind(dat,labeled_spectrograms)
  }
  dat
}

df_spectrograms <- process_files()

df_spectrograms$filename <- as.character(df_spectrograms$filename)
df_spectrograms$spectrograms <- as.matrix(df_spectrograms$spectrograms)

