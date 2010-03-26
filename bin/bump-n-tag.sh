#!/bin/sh

master=`git branch |grep "^\* master"`
if [[ -z $master ]]; then
    echo "bump-n-tag: can only bump versions on master"
    exit -1
fi

tag=`git describe --tag`
if [[ -z $tag ]]; then
    echo "bump-n-tag: no tag found"
    exit -1
fi

echo Currently at version \"$tag\"
echo -n "Please enter new version: "
read version

sed -ie "s/:version.*/:version \"$version\"/" discworld.asd
git add discworld.asd
git commit -m "update defsystem for $version"
git tag -s -m "$version" $version
