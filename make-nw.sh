cat <<!
NWB=`pwd`
export NOREBO_PATH=".:\${NWHOME:-.}:\$NWB/Plus:\$NWB/Kernel"
exec rlwrap \$NWB/zorebo "\$@" 2>/dev/null # blot out the LED displays in a blunt way, for now
!
