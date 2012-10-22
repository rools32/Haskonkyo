module Config (keyList) where

import Types(KeyBind(..))
--import Keys(mapKeys)

keyList =
  [ KeyBind 'z'  "poweroff"
  , KeyBind 'a'  "poweron"
  , KeyBind ' '  "pause"
  , KeyBind 'j'  "down"
  , KeyBind 'k'  "up"
  , KeyBind 'q'  "quit"
  , KeyBind 'h'  "left"
  , KeyBind 'l'  "right"
  , KeyBind 'n'  "net"
  , KeyBind 't'  "top"
  , KeyBind '\r' "enter"
  , KeyBind 'u'  "back"
  , KeyBind '>'  "next"
  , KeyBind '<'  "previous"
  , KeyBind '+'  "volume_up"
  , KeyBind '-'  "volume_down"
  , KeyBind '0'  "0"
  , KeyBind '1'  "1"
  , KeyBind '2'  "2"
  , KeyBind '3'  "3"
  , KeyBind '4'  "4"
  , KeyBind '5'  "5"
  , KeyBind '6'  "6"
  , KeyBind '7'  "7"
  , KeyBind '8'  "8"
  , KeyBind '9'  "9"
  , KeyBind ':'  "command"
  , KeyBind 'i'  "keyboard"
  , KeyBind 'r'  "refresh"
  , KeyBind '/'  "searchOnSpotify"
  ]
