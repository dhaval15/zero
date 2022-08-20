cd ~/Dev/OrgFiles/Discover/
results=$(sqlite3 index.db "SELECT * from nodes WHERE genre = '\"$1\"'")
node=$(echo "$results"| awk -F '|' '{print $2}' | awk -F '"' '{print $2}' | fzf)
file=$(sqlite3 index.db "SELECT * from nodes WHERE genre = '\"$1\"' AND title = '\"$node\"'" | awk -F '|' '{print $3}'| awk -F '"' '{print $2}')
"$2" "$file"
