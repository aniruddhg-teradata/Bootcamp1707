# install gdal on AMI linux

sudo yum -y update
sudo yum-config-manager --enable epel
sudo yum -y install make automake gcc gcc-c++ libcurl-devel proj-devel geos-devel
cd /tmp
curl -L http://download.osgeo.org/gdal/2.0.0/gdal-2.0.0.tar.gz | tar zxf -
cd gdal-2.0.0/
./configure --prefix=/usr/local
make -j4
sudo make install
sudo echo "/usr/local" > /etc/ld.so.conf.d/usrlocal.conf
sudo ldconfig