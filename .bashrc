export DISPLAY=localhost:0.0
export DISPLAY=$(grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0
