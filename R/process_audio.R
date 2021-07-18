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

songfiles <- df %>% filter(type == "song") %>% select(filename)
#df <- df %>% filter(species %in% top_5$species)

specie <- "comrav"

dir <- paste(getwd(), 'data/xeno-canto-bird-recordings-extended-a-m/A-M',specie, sep="/")
# files <- list.files(path=dir)

### FILTER ONLY FILES THAT HAVE BEEN LABELED AS SONG
files <- df %>% filter(ebird_code == specie, type == "song")
files <- na.omit(files)
files <- files$filename[1:2]


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
      spec_out = TRUE, nbands=128, hoptime=size/128)$aspectrum

   ### Stride through the spectrogram matrix at 64X64 "pixel box" intervals
   ### and find the one that has the greatest standard deviation
    boxes <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
                      c("h","h_end","v","v_end","sta"))
    print(dim(m))
    
    s <- seq(1,104,31)
    for (v in s) {
      v_end <- v + 31
      box <- try(m[v:v_end,1:128])
      if(inherits(box, "try-error")) {
        next
      }
      boxes <- boxes %>% add_row(h=v,h_end=v_end,v=1,v_end=128,sta=sd(box))
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
  samples
}


process_files <- function() {
  dat <- data.frame(filenames=c(), spectrograms=c())
  m_labels <- paste("m", seq(1,32 * 128, 1),sep="")
  for (file in files) {
    print(file)
    
    spectrograms <- gen_file_specs(file)
    wide_spectrograms <- data.frame()
    filename <- c()
    for (spec in spectrograms) {
      df <- data.frame(values=as.vector(t(spec)))
      df$m_labels <- m_labels
      df <- pivot_wider(df, names_from = m_labels, values_from = values)
      wide_spectrograms <- rbind(wide_spectrograms, df)
      filename <- c(filename, file)
    }
    labeled_spectrograms <- cbind(filename, wide_spectrograms)
    dat <- rbind(dat,labeled_spectrograms)
  }
  dat
}



df_spectrograms <- process_files()

df_spectrograms$filename <- as.character(df_spectrograms$filename)

### image(sample(df_spectrograms$spectrogram,1)[[1]],col=topo.colors(256))
