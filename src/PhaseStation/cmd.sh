#! /bin/sh
usr_local_lib=/scratch1/myphasestation/usr/lib64
# mount -o loop /scratch2/openSUSE-12.2-DVD-x86_64.iso /mnt
# cp -a /usr/share/doc/packages/kiwi/examples/suse-12.2/suse-live-iso/* .
rm -rf /scratch1/myphasestation/*
rm -rf /scratch1/myphasestation-result
kiwi --prepare /scratch1/PhaseStation --root /scratch1/myphasestation --force-new-root
# my modifications
cp /usr/local/bin/phase* /scratch1/myphasestation/usr/local/bin
cp /usr/local/hdf5/lib/libhdf5.so.7 /scratch1/myphasestation/usr/lib64
cp /opt/intel/composer_xe_2013.1.117/compiler/lib/intel64/libifport.so.5 /scratch1/myphasestation/usr/lib64
cp /opt/intel/composer_xe_2013.1.117/compiler/lib/intel64/libifcore.so.5 /scratch1/myphasestation/usr/lib64
cp /opt/intel/composer_xe_2013.1.117/compiler/lib/intel64/libimf.so      /scratch1/myphasestation/usr/lib64
cp /opt/intel/composer_xe_2013.1.117/compiler/lib/intel64/libsvml.so     /scratch1/myphasestation/usr/lib64
cp /opt/intel/composer_xe_2013.1.117/compiler/lib/intel64/libirc.so      /scratch1/myphasestation/usr/lib64
cp /opt/intel/composer_xe_2013.1.117/compiler/lib/intel64/libintlc.so.5  /scratch1/myphasestation/usr/lib64
echo "/usr/local/lib" > /scratch1/myphasestation/etc/ld.so.conf.d/usr_local_lib.conf

cp /usr/lib64/libqwt.so.6 /scratch1/myphasestation/usr/lib64

cp /usr/local/root/lib/libCore.so /scratch1/myphasestation/usr/lib64
cp /usr/local/root/lib/libCint.so /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libRIO.so  /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libNet.so  /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libHist.so     /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libGraf.so     /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libGraf3d.so   /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libGpad.so     /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libTree.so     /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libRint.so     /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libPostscript.so /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libMatrix.so     /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libPhysics.so    /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libMathCore.so   /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libThread.so     /scratch1/myphasestation/usr/lib64 
cp /usr/local/root/lib/libMinuit.so     /scratch1/myphasestation/usr/lib64 

mkdir -p /scratch1/myphasestation/home/phaseuser/phase/data
cp /home/flechsig/phase/src/examples/*.phase /scratch1/myphasestation/home/phaseuser/phase/data
cp /home/flechsig/phase/src/examples/po-example1/* /scratch1/myphasestation/home/phaseuser/phase/data
mv /scratch1/myphasestation/home/phaseuser/phase/data/EZRE_GB_5000.DAT /scratch1/myphasestation/home/phaseuser/phase/data/test_5000.s4a
mv /scratch1/myphasestation/home/phaseuser/phase/data/EZIM_GB_5000.DAT /scratch1/myphasestation/home/phaseuser/phase/data/test_5000.s4b
cp /scratch1/myphasestation/home/phaseuser/phase/data/test_5000.s4a /scratch1/myphasestation/home/phaseuser/phase/data/test_5000.s4c
cp /scratch1/myphasestation/home/phaseuser/phase/data/test_5000.s4b /scratch1/myphasestation/home/phaseuser/phase/data/test_5000.s4d

mkdir -p /scratch1/myphasestation/home/phaseuser/.config/autostart
mkdir -p /scratch1/myphasestation/home/phaseuser/Desktop
cp ./phaseqt.desktop /scratch1/myphasestation/home/phaseuser/.config/autostart
cp ./phaseqt.desktop /scratch1/myphasestation/home/phaseuser/Desktop

chown -R 1001.100 /scratch1/myphasestation/home/phaseuser




kiwi --create /scratch1/myphasestation --type iso -d /scratch1/myphasestation-result 
# qemu-system-x86_64 -cdrom /scratch1/mytuxlive-result/ -m 2048
