# CobaltParser
 ## Cobalt Strike beacon*.log parser input and output
 ### Актуальный билд (исходники в разработке) [download](https://github.com/lartsev1337/Cobalt-Strike-Parser/releases/download/CobaltStrike_beacon_parser/CobaltParser.exe)
 Укажите папку в которой собраны логи beacon*.log, программа начнет искать файлы рекурсивно, в этой папке и пренадлежащей ей подпапками.
  
 По итогу в той папке которую мы указали будет создан CSV файл, в котором будут колоки:
 
 | HackerNickname | InputTime | InputCommand | Task | Output/ErrorTime | Output/ErrorResult |
 |:---------|:--------:|:--------:|:--------:|:--------:|---------:|
 | lartsev | 13/31 13:13:13 | run net group "domain admins" \/dom | <T1059> Tasked beacon to run: net group "domain admins" \/dom| 13/31 14:14:14 | чёта-там |

 
 HackerNickname, InputTime, InputCommand, Task, Output/ErrorTime, Output/ErrorResult
 
 + HackerNickname - никнейм хакера
 
 + InputTime - время отдачи команды
 
 + InputCommand - команда
 
 + Task - задача
 
 + Output/ErrorTime - время выполнения и получения ответа 
 
 + Output/ErrorResult - ответ хакеру
 
