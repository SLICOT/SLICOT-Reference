export D=$(date +"%Y-%m-%d")
sed -i -e "s,^date-released: .*\$,date-released: $D,g" CITATION.cff

