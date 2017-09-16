library(XML)
library(data.table)


getArtistSongs <- function(artist, delay_min = 0, delay_max = 0, unique_songs = T,
                           BASE_SITE = 'http://xxxxxx.com') {
  # given an artist name gets the song information for all albums
  # 
  # args:
  #   artist: the artist name as character
  #   delay_min, delay_max: a random delay time is waited between hhtp requests
  #     (because some sites might ban you ifyou request too much too fast)
  #   unique_songs: if true, a song is included only on the 1st time it was released 
  #   BASE_SITE: the base url of a known lyrics source
  # returns:
  #   a list, each element contains:
  #     - a data.table song_info with album, year, song name and link to lyrics
  #     - a character vector lyrics, each element is a line of the song
  
  page <- paste0(BASE_SITE, '/xxxxxx/', gsub(' ', '_', artist))
  
  doc.html <-  htmlTreeParse(page, useInternal = TRUE)
  
  
  nodes <- getNodeSet(doc.html, "//span[@class='mw-headline']/a")
  album_links <- sapply(nodes, function(x) xmlAttrs(x)['href'])
  album_titles <- sapply(nodes, function(x) xmlAttrs(x)['title'])
  
  albums <- data.table(title = gsub('(^.*:)(.*)\\((.*)\\)$', '\\2', album_titles),
                       year = gsub('(^.*:)(.*)\\((.*)\\)$', '\\3', album_titles),
                       link = paste0(BASE_SITE, album_links))
  
  songs <- vector('list', nrow(albums))
  message('Getting list of songs per album')
  for (alb in 1:length(songs)) {
    if (alb%%5 == 0) {message('album ', alb, ' of ', length(songs))}
    
    Sys.sleep(runif(1, delay_min, delay_max))
    doc.html <-  htmlTreeParse(albums[alb, link], useInternal = TRUE)
    nodes <- getNodeSet(doc.html, "//ol/li/b/a")
    song_links <- sapply(nodes, function(x) xmlAttrs(x)['href'])
    song_titles <- sapply(nodes, function(x) xmlAttrs(x)['title'])
    
    songs[[alb]] <- data.table(album = albums[alb, title],
                                     year = albums[alb, year],
                                     song = gsub('(^.*:)(.*)','\\2', song_titles),
                                     link = paste0(BASE_SITE, song_links))
  }
  songs <- rbindlist(songs, use.names = T)
  songs[, album := trimws(album)]
  songs[, song:= trimws(song)]
  songs <- songs[!grepl('redlink=1', link), ]
  songs[, year := as.numeric(year)]
  if (unique_songs) {
    songs <- unique(songs)
    songs <- songs[ , .SD[which.min(year)], by = .(song, link)][, .(album, year, song, link)]
  }
  
  message('Getting song lyrics')
  lyric_list <- vector('list', nrow(songs))
  for (i in 1:length(lyric_list)) {
    if (i%%5 == 0) {message('song ', i, ' of ', length(lyric_list))}
    
    Sys.sleep(runif(1, delay_min, delay_max))
    doc.html <-  htmlTreeParse(songs[i, link], useInternal = TRUE)
    nodes <- getNodeSet(doc.html, "//div[@class='lyricbox']")
    this_lyrics <- lapply(nodes, function(x) xpathSApply(x,".//text()", xmlValue))[[1]]
    this_lyrics <- this_lyrics[this_lyrics != '\n']
    lyric_list[[i]] <- list(song_info = songs[i, ],
                            lyrics = this_lyrics)
  }
  
  message('Done!!')
  return(lyric_list)
  
}




