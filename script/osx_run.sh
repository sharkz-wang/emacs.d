#!/bin/bash
osascript - "$@" <<EOF
on run argv
tell application "iTerm"
	activate
	set new_term to (create window with default profile)
	tell new_term
		tell the current session
			set cmd to ""
			repeat with arg in argv
				set cmd to cmd & arg & " "
			end repeat
			write text cmd & "; read '?\nPress enter to exit...'; exit 0"
		   end tell
	   end tell
   end tell
end run
EOF
