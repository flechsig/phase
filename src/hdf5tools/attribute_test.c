/* File      : /afs/psi.ch/user/f/flechsig/c/guralp-tools/gcf2h5/attribute_test.c */
/* Date      : <2025-02-11 10:45:34 flechsig>  */
/* Time-stamp: <2025-02-11 13:50:30 flechsig>  */
/* Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104; */

// test ordered attributes in hdf5

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <H5public.h>
#include <H5Apublic.h>
#include <H5Fpublic.h>
#include <H5Tpublic.h>
#include <H5Spublic.h>
#include <H5Ppublic.h>
#include <H5Gpublic.h>

//#define ORDER
#define FILE_ATTRIBUTES
#define GROUP_ATTRIBUTES
#define DATASET_ATTRIBUTES

// Callback function to print attribute names in order
herr_t print_attr_name(hid_t loc_id, const char *attr_name, const H5A_info_t *ainfo, void *opdata) {
    (void) loc_id;
    (void) ainfo;
    (void) opdata;
    
    printf("Attribute: %s\n", attr_name);
    return 0;  // Continue iteration
}
// Files seem to do not support attribute creation order tracking

int main()
{
   
   hid_t file_id, dataspace_id, dataspace2_id, dataset_id, type_str_id, plist_group_id, plist_file_id, plist_dataset_id, group_id, attr_id;
   hsize_t dims[1];
   // Create attributes in a specific order
   //const char *attr1 = "First Attribute";
   //const char *attr2 = "Second Attribute";
   //const char *attr3 = "Third Attribute";
   double double_attr = 0.33, dlist[4]= {1.1, 2.2, 3.3, 4.4};
   const char *keys[] = {"xattr1", "attr2", "attr3"};
   const char *values[] = {"value1", "value2", "value3"}; 
   int i, int_attr = 33, nattrs= 3; 

   dims[0]= 4;

// Create property lists to enable attribute creation order tracking
   plist_group_id   = H5Pcreate(H5P_GROUP_CREATE);                             // group creation property list class
   plist_file_id    = H5Pcreate(H5P_FILE_CREATE);
   plist_dataset_id = H5Pcreate(H5P_DATASET_CREATE);
   
   H5Pset_attr_creation_order(plist_group_id,   H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED);  // fill class
   H5Pset_attr_creation_order(plist_dataset_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED);
   H5Pset_attr_creation_order(plist_file_id,    H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED);  

   // Create HDF5 file
   file_id = H5Fcreate("ordered_utf8_attrs.h5", H5F_ACC_TRUNC, plist_file_id, H5P_DEFAULT);

   // Create a scalar dataspace for attributes, and dataspace for double data
   dataspace_id  = H5Screate(H5S_SCALAR);
   dataspace2_id = H5Screate_simple(1, dims, NULL);
   
   // Create a UTF-8 string type
   type_str_id = H5Tcopy(H5T_C_S1);
   H5Tset_size(type_str_id, H5T_VARIABLE);
   H5Tset_cset(type_str_id, H5T_CSET_UTF8);

#ifdef FILE_ATTRIBUTES
   for (i= 0; i< nattrs; i++)
     {
        attr_id = H5Acreate(file_id, keys[i], type_str_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attr_id, type_str_id, &values[i]);
	H5Aclose(attr_id);
     }
   attr_id = H5Acreate(file_id, "attr_4_double", H5T_NATIVE_DOUBLE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
   H5Awrite(attr_id, H5T_NATIVE_DOUBLE, &double_attr);
   H5Aclose(attr_id);
   attr_id = H5Acreate(file_id, "attr_4_int", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
   H5Awrite(attr_id, H5T_NATIVE_INT, &int_attr);
   H5Aclose(attr_id);
#endif
   
   // Create a group with attribute order tracking
   group_id = H5Gcreate(file_id, "/my_group", H5P_DEFAULT, plist_group_id, H5P_DEFAULT);

#ifdef GROUP_ATTRIBUTES
   for (i= 0; i< nattrs; i++)
     {
       	attr_id = H5Acreate(group_id, keys[i], type_str_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attr_id, type_str_id, &values[i]);
	H5Aclose(attr_id);
     }
   attr_id = H5Acreate(group_id, "xattr_4_double", H5T_NATIVE_DOUBLE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
   H5Awrite(attr_id, H5T_NATIVE_DOUBLE, &double_attr);
   H5Aclose(attr_id);
   attr_id = H5Acreate(group_id, "attr_4_int", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
   H5Awrite(attr_id, H5T_NATIVE_INT, &int_attr);
   H5Aclose(attr_id);
#endif
   
   // dataset
   dataset_id = H5Dcreate(group_id, "dataset", H5T_NATIVE_DOUBLE, dataspace2_id, H5P_DEFAULT, plist_dataset_id, H5P_DEFAULT);
   H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dlist);  // fill data

#ifdef DATASET_ATTRIBUTES
   for (i= 0; i< nattrs; i++)
     {
       attr_id = H5Acreate(dataset_id, keys[i], type_str_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
       H5Awrite(attr_id, type_str_id, &values[i]);
       H5Aclose(attr_id);
     }
   attr_id = H5Acreate(dataset_id, "xattr_4_double", H5T_NATIVE_DOUBLE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
   H5Awrite(attr_id, H5T_NATIVE_DOUBLE, &double_attr);
   H5Aclose(attr_id);
   attr_id = H5Acreate(dataset_id, "attr_4_int", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
   H5Awrite(attr_id, H5T_NATIVE_INT, &int_attr);
   H5Aclose(attr_id);
#endif
   
  
    // Iterate through attributes in creation order
    printf("Reading attributes in order:\n");
    H5Aiterate2(group_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL, print_attr_name, NULL);
   
    // Clean up resources
    
    H5Gclose(group_id);
    H5Pclose(plist_group_id);
    H5Pclose(plist_file_id);
    H5Pclose(plist_dataset_id);
    H5Tclose(type_str_id);
    H5Sclose(dataspace_id);
    H5Sclose(dataspace2_id);
    H5Dclose(dataset_id);
    H5Fclose(file_id);

    return 0;
} // end main



