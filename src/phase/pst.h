 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/pst.h */
 /* Date      : <10 Mar 14 14:20:23 flechsig>  */
 /* Time-stamp: <10 Mar 14 14:34:44 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifndef PST_H
#define PST_H
void check_2_m4_(struct map4 *);
void copySrc2Psd(struct BeamlineType *);
void fill_m4(struct BeamlineType *, struct map4 *);
void fill_xirp(struct BeamlineType *, struct integration_results *);
void getgeostr_(int *, int *, double *, double *, double *, double *, double *, double *, double *);
double getIntensityMax(struct PSDType *);
void pstc(struct BeamlineType *);
void pstc_i(int, struct BeamlineType *, struct map4 *, struct constants *);
void pstc_ii(int, struct BeamlineType *);
void Test4Grating(struct BeamlineType *);
void WritePsd(char *, struct PSDType *, int, int, struct BeamlineType *);
#endif  /*  PST_H */
