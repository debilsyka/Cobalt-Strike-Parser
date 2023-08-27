# CobaltParser
 ## Cobalt Strike Log parser input and output
 ### Current build [download](https://github.com/lartsev1337/Cobalt-Strike-Parser/releases/download/parser/CobaltParser.exe)
 Specify the folder where the beacon*.log logs are collected, the program will start searching for files recursively in this folder and its subfolders.

 ![gui](https://github.com/lartsev1337/Cobalt-Strike-Parser/assets/141585428/3e109620-4d6f-435f-9785-d05401f85dc3)

 As a result, in the folder we specified, a CSV file will be created with the following columns:
 
 | Metadata | MetaTime | HackerNickname | InputTime | InputCommand | Task | Output/ErrorTime | Output/ErrorResult |
 |:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|
 | 0.0.0.0 <- 0.0.0.0 ... ... | 13/31 12:12:12 | lartsev | 13/31 13:13:13 | run net group "domain admins" \/dom | <T1059> Tasked beacon to run: net group "domain admins" \/dom| 13/31 14:14:14 | easy-peasy |


 + Metadata - information about the connection and module

 + MetaTime - start time
 
 + HackerNickname - hacker's nickname
 
 + InputTime - time of command issuance
 
 + InputCommand - command
 
 + Task - task
 
 + Output/ErrorTime - execution time and response time
 
 + Output/ErrorResult - response to the hacker
 
