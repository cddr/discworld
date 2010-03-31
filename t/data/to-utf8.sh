
TMP=`mktemp `

for doc in $@
do
    echo "converting $doc to utf-8..."
    xsltproc xsl/to-utf8.xsl $doc >$TMP
    cp $TMP $doc
done

rm $TMP

