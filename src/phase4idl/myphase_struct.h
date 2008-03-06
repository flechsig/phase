/*   File      : /home/leitner/phase4idl/include/myphase_struct.h */
/*   Date      : <15 Mar 06 >  */
/*   Author    : Torsten.Leitner@email.de */


#ifndef __MYPHASE_STRUCT
#define __MYPHASE_STRUCT

struct  source4 {

  char 
      fsource4a[80], fsource4b[80], fsource4c[80],fsource4d[80];
  
  double 
	 xeyremin,xeyremax,dxeyre,
	 xeyimmin,xeyimmax,dxeyim,
	 yeyremin,yeyremax,dyeyre,
	 yeyimmin,yeyimmax,dyeyim,
	 
	 zeyre[256][256],zeyim[256][256],
	 
	 xezremin,xezremax,dxezre,
	 xezimmin,xezimmax,dxezim,
	 yezremin,yezremax,dyezre,
	 yezimmin,yezimmax,dyezim,
	 
       zezre[256][256],zezim[256][256],
	 
	 xlam;
	 
  int  ieyrex,ieyimx,ieyrey,ieyimy,
       iezrex,iezimx,iezrey,iezimy;

};  // source4


#endif 
