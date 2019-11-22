#! /bin/sh
# create symbolic links for suse linux
# with root priveliges set mypath to /usr/bin
# otherwise perhaps $home/bin and put $home/bin in your path
mypath=/usr/bin
ln -s /usr/bin/qmake-qt5    ${mypath}/qmake
ln -s /usr/bin/moc-qt5      ${mypath}/moc
ln -s /usr/bin/uic-qt5      ${mypath}/uic
ln -s /usr/bin/rcc-qt5      ${mypath}/rcc
ln -s /usr/bin/lrelease-qt5 ${mypath}/lrelease
ln -s /usr/bin/lupdate-qt5  ${mypath}/lupdate

