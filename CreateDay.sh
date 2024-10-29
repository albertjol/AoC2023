#!/bin/bash
cp -R Template $1
cd $1
mv AdvCodeTemplate.ico AdvCode$1.ico
mv AdvCodeTemplate.lpi AdvCode$1.lpi
mv AdvCodeTemplate.lpr AdvCode$1.lpr
mv uAdvCodeTemplate.lfm uAdvCode$1.lfm
mv uAdvCodeTemplate.pas uAdvCode$1.pas
sed -i '' -e "s/AdvCodeTemplate/AdvCode$1/g" *
cd ..
echo AdvCode$1 >> .gitignore
