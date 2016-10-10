require 'irb/completion'
require 'wirble'

IRB.conf[:SAVE_HISTORY] = 10000
IRB.conf[:AUTO_INDENT] = true

Wirble.init
Wirble.colorize
