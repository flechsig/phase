// genesis h5 output from Sven Reiche

#ifdef GENESIS_HDF5

#include "HDF5base.h"

HDF5Base::HDF5Base(){}
HDF5Base::~HDF5Base(){}

//------------------------
// writing procedures

void HDF5Base::writeDataDouble(hid_t fid, char *name, double *data, int size)
{
  hsize_t dims[1];
  dims[0]=size;
  hid_t dataspace_id=H5Screate_simple(1,dims,NULL);
  hid_t dataset_id=H5Dcreate(fid,name,H5T_NATIVE_DOUBLE,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

void HDF5Base::writeDataInt(hid_t fid, char *name, int *data, int size)
{
  hsize_t dims[1];
  dims[0]=size;
  hid_t dataspace_id=H5Screate_simple(1,dims,NULL);
  hid_t dataset_id=H5Dcreate(fid,name,H5T_NATIVE_INT,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

//--------------------- 
// reading procedures


void HDF5Base::readDataDouble(hid_t fid, char *name, double *data, int size)
{

  hsize_t dims[1];
  dims[0]=size;
  hid_t dataspace_id=H5Screate_simple(1,dims,NULL);
  hid_t dataset_id=H5Dopen(fid,name,H5P_DEFAULT);
  H5Dread(dataset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  H5Dclose(dataset_id);     
  H5Sclose(dataspace_id);
  return;
}

void HDF5Base::readDataInt(hid_t fid, char *name, int *data, int size)
{
  hsize_t dims[1];
  dims[0]=size;
  hid_t dataspace_id=H5Screate_simple(1,dims,NULL);
  hid_t dataset_id=H5Dopen(fid,name,H5P_DEFAULT);
  H5Dread(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  H5Dclose(dataset_id);     
  H5Sclose(dataspace_id);
  return;
}


int HDF5Base::getDatasetSize(hid_t fid, char *name)
{

  hsize_t dims[1],maxdims[1];
  hid_t  dsid=H5Dopen(fid,name,H5P_DEFAULT);
  hid_t spaceid=H5Dget_space(dsid);
  H5Sget_simple_extent_dims(spaceid,dims,maxdims);
  
  return dims[0];

}

#endif
 
