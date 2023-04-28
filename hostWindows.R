require(shiny)
folder_address = "C:/Users/ARM-Ismail/Desktop/ARM Application"

x <- system("ipconfig", intern=TRUE)
x
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)
print(paste0("the Shiny Web application runs on: http://", ip, ":1234/"))

runApp(folder_address, launch.browser=TRUE, port = 1234, host = ip)
