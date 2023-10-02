#! /bin/awk -f

{
if ($1 == ">"){
    print "commit_name := \"" $2 "\"";
    printf "commit_description := \"";
    printf $4;
    for(i=5;i<=NF;i++){printf " %s", $i} printf "\"\n";
    }
}