 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/fkoempi/fkoempi.h */
 /* Date      : <13 Aug 14 14:03:44 flechsig>  */
 /* Time-stamp: <13 Aug 14 14:06:35 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

/* constants */
#define MAXDIM       2048
#define PARFNAME     "fkoempi.par"
#define BASEOFNAME   "fout"


/* type definitions */
struct extends
{
  double y_min;
  double y_max;
  double z_min;
  double z_max;
};

struct complex
{
  double re, im;
};



/* function prototypes */
void all_load_parameters(const char *parfname);
void all_load_fields();
void master_save_fields(const char *baseofname, int ny, int nz);
void slave_load_surface(const char *surffilename, int *ny, int *nz, double sy[], double sz[]);

void build_dest_axis(const struct extends *e, int ny, int nz, double sy[], double sz[]);

void master_propagate(int nslaves);
void slave_propagate_1(double angle);
void slave_propagate_2(double angle);
void slave_propagate_10();
/* end */
