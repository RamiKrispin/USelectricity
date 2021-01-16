args <- commandArgs(trailingOnly = TRUE)

print(args[1])

if(args[1] == ".test1"){
  print("Good")
} else {
  print("Bad")
}
