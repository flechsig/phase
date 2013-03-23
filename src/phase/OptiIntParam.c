void FindIntRange(struct BeamlineType *bl)
{
//  int fr_mat[2][2]; // matrix for utilisation results
  double yedge = 10.0E-3;   // start values
  double zedge = 10.0E-3;

  int i, n;  
  int ianzy0;
  double y1, y2, z1, z2;
  struct PSDType *psdp;  
  struct PSImageType *psip;
  struct PSImageType backup_psip; 
  
  #ifdef DEBUG  
  printf("DEBUG: enter [%s]:FindIntRange()\n", __FILE__);
  #endif 

  // backup input struct in case we can't obtain reasonable results
  psip = (struct PSImageType *)bl->RTSource.Quellep;
  memcpy(&backup_psip, psip, sizeof(struct PSImageType));
  
  // use only one gridpoint and central ray for test procedure
  psip->ymin = psip->ymax = psip->zmin = psip->zmax = 0.0;  
  psip->iy = psip->iz = 1;

  // simple loop approach (fixed step size)
  for (i=0; i<100; i++)
  {
    int nonzeros = 0;

    y1 = -yedge+i*1E-4;
    y2 = -y1;
    
    bl->BLOptions.xi.ymin = y1;
    bl->BLOptions.xi.ymax = y2;
    
    PST(bl);

    // pointer to simpsons data and vector length
    psdp = (struct PSDType *)bl->RESULT.RESp;
    ianzy0 = bl->BLOptions.xi.ianzy0;
    
    // determine unused (near zero) percentage of result
    for (n=0; n<ianzy0; n++) 
    {
      double c2y = psdp->simpre[8*n+5];
      if (fabs(c2y) >= 1E-10) nonzeros++;
    }

    printf("Autorange: pass %d, utilisation: %d\n", i, nonzeros);

    if (nonzeros == ianzy0) break;           
  }  


  // loop for dz
  // dz should be small enough so that plots contains only zeroes,
  // as the plots correspond to left/right-most edge 
  for (i=0; i<100; i++)
  {
    int nonzeros_l = 0;
    int nonzeros_r = 0;
    int nonzeros_i = 0;

    z1 = -zedge+i*1E-4;
    z2 = -z1;
    
    bl->BLOptions.xi.zmin = z1;
    bl->BLOptions.xi.zmax = z2;
    
    PST(bl);

    // pointer to simpsons data and vector length
    psdp = (struct PSDType *)bl->RESULT.RESp;
    ianzy0 = bl->BLOptions.xi.ianzy0;
    
    // determine unused (near zero) percentage of result
    for (n=0; n<ianzy0; n++) 
    {
      double c1y = psdp->simpre[8*n+4]; // upper left
      double c3y = psdp->simpre[8*n+6]; // lower left
      double c4y = psdp->simpre[8*n+7]; // lower right (yellow)

      if (fabs(c1y) >= 1E-10) nonzeros_l++;
      if (fabs(c3y) >= 1E-10) nonzeros_r++;
      if (fabs(c3y) >= 1E-10) nonzeros_i++;
    }

    printf("Autorange: pass %d, utilisation: %d, %d, %d\n", i, nonzeros_l, nonzeros_r, nonzeros_i);

    if ((nonzeros_l == 0) && (nonzeros_r == 0) && (nonzeros_i == ianzy0)) break;      
    if ((nonzeros_l > 0) || (nonzeros_r > 0)) {z1 -= 1E-4; z2 = -z1; break;};          
  }  


// test fill to see which array is which plot
/*    for (n=0; n<ianzy0; n++) 
    {      
      double c2y = psdp->simpre[8*n+5];
      psdp->simpre[8*n+5] = n;
//      if (fabs(c1y) >= 1E-10) nonzeros++;
    }
*/

  bl->BLOptions.xi.zmin = z1;
  bl->BLOptions.xi.zmax = z2;
  printf("Autorange: best set found: (%.2f, %.2f, %.2f, %.2f)mrad\n", y1*1E3, y2*1E3, z1*1E3, z2*1E3);


  //restoring range struct
  memcpy(psip, &backup_psip, sizeof(struct PSImageType));
}
