cat <<!
NWB=`pwd`
export NOREBO_PATH=".:\${NWHOME:-.}:\$NWB/Plus:\$NWB/Kernel"
exec \$NWB/zorebo "\$@"
!
