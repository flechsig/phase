/* wdfgmapm17.f -- translated by f2c (version 20100827).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* $$$ $Source$ */
/* $$$ $Date$ */
/* $$$ $Revision$ */
/* $$$ $Author$ */
/* Subroutine */ int subm17_(doublereal *p1c, doublereal *y, doublereal *z__, 
	doublereal *p1rc)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8, d__9, d__10, 
	    d__11, d__12, d__13, d__14, d__15, d__16, d__17, d__18, d__19, 
	    d__20, d__21, d__22, d__23, d__24, d__25, d__26, d__27, d__28, 
	    d__29, d__30, d__31, d__32, d__33, d__34, d__35, d__36, d__37, 
	    d__38, d__39, d__40, d__41, d__42, d__43, d__44, d__45, d__46, 
	    d__47, d__48, d__49, d__50;

/* Computing 7th power */
    d__1 = *y, d__2 = d__1, d__1 *= d__1, d__2 *= d__1;
/* Computing 6th power */
    d__3 = *y, d__3 *= d__3;
/* Computing 6th power */
    d__4 = *y, d__4 *= d__4;
/* Computing 5th power */
    d__5 = *y, d__6 = d__5, d__5 *= d__5;
/* Computing 2nd power */
    d__7 = *z__;
/* Computing 5th power */
    d__8 = *y, d__9 = d__8, d__8 *= d__8;
/* Computing 5th power */
    d__10 = *y, d__11 = d__10, d__10 *= d__10;
/* Computing 4th power */
    d__12 = *y, d__12 *= d__12;
/* Computing 3rd power */
    d__13 = *z__;
/* Computing 4th power */
    d__14 = *y, d__14 *= d__14;
/* Computing 2nd power */
    d__15 = *z__;
/* Computing 4th power */
    d__16 = *y, d__16 *= d__16;
/* Computing 4th power */
    d__17 = *y, d__17 *= d__17;
/* Computing 3rd power */
    d__18 = *y;
/* Computing 4th power */
    d__19 = *z__, d__19 *= d__19;
/* Computing 3rd power */
    d__20 = *y;
/* Computing 3rd power */
    d__21 = *z__;
/* Computing 3rd power */
    d__22 = *y;
/* Computing 2nd power */
    d__23 = *z__;
/* Computing 3rd power */
    d__24 = *y;
/* Computing 3rd power */
    d__25 = *y;
/* Computing 2nd power */
    d__26 = *y;
/* Computing 5th power */
    d__27 = *z__, d__28 = d__27, d__27 *= d__27;
/* Computing 2nd power */
    d__29 = *y;
/* Computing 4th power */
    d__30 = *z__, d__30 *= d__30;
/* Computing 2nd power */
    d__31 = *y;
/* Computing 3rd power */
    d__32 = *z__;
/* Computing 2nd power */
    d__33 = *y;
/* Computing 2nd power */
    d__34 = *z__;
/* Computing 2nd power */
    d__35 = *y;
/* Computing 2nd power */
    d__36 = *y;
/* Computing 6th power */
    d__37 = *z__, d__37 *= d__37;
/* Computing 5th power */
    d__38 = *z__, d__39 = d__38, d__38 *= d__38;
/* Computing 4th power */
    d__40 = *z__, d__40 *= d__40;
/* Computing 3rd power */
    d__41 = *z__;
/* Computing 2nd power */
    d__42 = *z__;
/* Computing 7th power */
    d__43 = *z__, d__44 = d__43, d__43 *= d__43, d__44 *= d__43;
/* Computing 6th power */
    d__45 = *z__, d__45 *= d__45;
/* Computing 5th power */
    d__46 = *z__, d__47 = d__46, d__46 *= d__46;
/* Computing 4th power */
    d__48 = *z__, d__48 *= d__48;
/* Computing 3rd power */
    d__49 = *z__;
/* Computing 2nd power */
    d__50 = *z__;
    p1rc[0] = p1c[7] * (d__2 * (d__1 * d__1)) + p1c[14] * (d__3 * (d__3 * 
	    d__3)) * *z__ + p1c[6] * (d__4 * (d__4 * d__4)) + p1c[21] * (d__6 
	    * (d__5 * d__5)) * (d__7 * d__7) + p1c[13] * (d__9 * (d__8 * d__8)
	    ) * *z__ + p1c[5] * (d__11 * (d__10 * d__10)) + p1c[28] * (d__12 *
	     d__12) * (d__13 * (d__13 * d__13)) + p1c[20] * (d__14 * d__14) * 
	    (d__15 * d__15) + p1c[12] * (d__16 * d__16) * *z__ + p1c[4] * (
	    d__17 * d__17) + p1c[35] * (d__18 * (d__18 * d__18)) * (d__19 * 
	    d__19) + p1c[27] * (d__20 * (d__20 * d__20)) * (d__21 * (d__21 * 
	    d__21)) + p1c[19] * (d__22 * (d__22 * d__22)) * (d__23 * d__23) + 
	    p1c[11] * (d__24 * (d__24 * d__24)) * *z__ + p1c[3] * (d__25 * (
	    d__25 * d__25)) + p1c[42] * (d__26 * d__26) * (d__28 * (d__27 * 
	    d__27)) + p1c[34] * (d__29 * d__29) * (d__30 * d__30) + p1c[26] * 
	    (d__31 * d__31) * (d__32 * (d__32 * d__32)) + p1c[18] * (d__33 * 
	    d__33) * (d__34 * d__34) + p1c[10] * (d__35 * d__35) * *z__ + p1c[
	    2] * (d__36 * d__36) + p1c[49] * *y * (d__37 * (d__37 * d__37)) + 
	    p1c[41] * *y * (d__39 * (d__38 * d__38)) + p1c[33] * *y * (d__40 *
	     d__40) + p1c[25] * *y * (d__41 * (d__41 * d__41)) + p1c[17] * *y 
	    * (d__42 * d__42) + p1c[9] * *y * *z__ + p1c[1] * *y + p1c[56] * (
	    d__44 * (d__43 * d__43)) + p1c[48] * (d__45 * (d__45 * d__45)) + 
	    p1c[40] * (d__47 * (d__46 * d__46)) + p1c[32] * (d__48 * d__48) + 
	    p1c[24] * (d__49 * (d__49 * d__49)) + p1c[16] * (d__50 * d__50) + 
	    p1c[8] * *z__ + p1c[0];
/* Computing 6th power */
    d__1 = *y, d__1 *= d__1;
/* Computing 5th power */
    d__2 = *y, d__3 = d__2, d__2 *= d__2;
/* Computing 5th power */
    d__4 = *y, d__5 = d__4, d__4 *= d__4;
/* Computing 4th power */
    d__6 = *y, d__6 *= d__6;
/* Computing 2nd power */
    d__7 = *z__;
/* Computing 4th power */
    d__8 = *y, d__8 *= d__8;
/* Computing 4th power */
    d__9 = *y, d__9 *= d__9;
/* Computing 3rd power */
    d__10 = *y;
/* Computing 3rd power */
    d__11 = *z__;
/* Computing 3rd power */
    d__12 = *y;
/* Computing 2nd power */
    d__13 = *z__;
/* Computing 3rd power */
    d__14 = *y;
/* Computing 3rd power */
    d__15 = *y;
/* Computing 2nd power */
    d__16 = *y;
/* Computing 4th power */
    d__17 = *z__, d__17 *= d__17;
/* Computing 2nd power */
    d__18 = *y;
/* Computing 3rd power */
    d__19 = *z__;
/* Computing 2nd power */
    d__20 = *y;
/* Computing 2nd power */
    d__21 = *z__;
/* Computing 2nd power */
    d__22 = *y;
/* Computing 2nd power */
    d__23 = *y;
/* Computing 5th power */
    d__24 = *z__, d__25 = d__24, d__24 *= d__24;
/* Computing 4th power */
    d__26 = *z__, d__26 *= d__26;
/* Computing 3rd power */
    d__27 = *z__;
/* Computing 2nd power */
    d__28 = *z__;
/* Computing 6th power */
    d__29 = *z__, d__29 *= d__29;
/* Computing 5th power */
    d__30 = *z__, d__31 = d__30, d__30 *= d__30;
/* Computing 4th power */
    d__32 = *z__, d__32 *= d__32;
/* Computing 3rd power */
    d__33 = *z__;
/* Computing 2nd power */
    d__34 = *z__;
    p1rc[8] = p1c[518] * (d__1 * (d__1 * d__1)) + p1c[525] * (d__3 * (d__2 * 
	    d__2)) * *z__ + p1c[517] * (d__5 * (d__4 * d__4)) + p1c[532] * (
	    d__6 * d__6) * (d__7 * d__7) + p1c[524] * (d__8 * d__8) * *z__ + 
	    p1c[516] * (d__9 * d__9) + p1c[539] * (d__10 * (d__10 * d__10)) * 
	    (d__11 * (d__11 * d__11)) + p1c[531] * (d__12 * (d__12 * d__12)) *
	     (d__13 * d__13) + p1c[523] * (d__14 * (d__14 * d__14)) * *z__ + 
	    p1c[515] * (d__15 * (d__15 * d__15)) + p1c[546] * (d__16 * d__16) 
	    * (d__17 * d__17) + p1c[538] * (d__18 * d__18) * (d__19 * (d__19 *
	     d__19)) + p1c[530] * (d__20 * d__20) * (d__21 * d__21) + p1c[522]
	     * (d__22 * d__22) * *z__ + p1c[514] * (d__23 * d__23) + p1c[553] 
	    * *y * (d__25 * (d__24 * d__24)) + p1c[545] * *y * (d__26 * d__26)
	     + p1c[537] * *y * (d__27 * (d__27 * d__27)) + p1c[529] * *y * (
	    d__28 * d__28) + p1c[521] * *y * *z__ + p1c[513] * *y + p1c[560] *
	     (d__29 * (d__29 * d__29)) + p1c[552] * (d__31 * (d__30 * d__30)) 
	    + p1c[544] * (d__32 * d__32) + p1c[536] * (d__33 * (d__33 * d__33)
	    ) + p1c[528] * (d__34 * d__34) + p1c[520] * *z__ + p1c[512];
/* Computing 5th power */
    d__1 = *y, d__2 = d__1, d__1 *= d__1;
/* Computing 4th power */
    d__3 = *y, d__3 *= d__3;
/* Computing 4th power */
    d__4 = *y, d__4 *= d__4;
/* Computing 3rd power */
    d__5 = *y;
/* Computing 2nd power */
    d__6 = *z__;
/* Computing 3rd power */
    d__7 = *y;
/* Computing 3rd power */
    d__8 = *y;
/* Computing 2nd power */
    d__9 = *y;
/* Computing 3rd power */
    d__10 = *z__;
/* Computing 2nd power */
    d__11 = *y;
/* Computing 2nd power */
    d__12 = *z__;
/* Computing 2nd power */
    d__13 = *y;
/* Computing 2nd power */
    d__14 = *y;
/* Computing 4th power */
    d__15 = *z__, d__15 *= d__15;
/* Computing 3rd power */
    d__16 = *z__;
/* Computing 2nd power */
    d__17 = *z__;
/* Computing 5th power */
    d__18 = *z__, d__19 = d__18, d__18 *= d__18;
/* Computing 4th power */
    d__20 = *z__, d__20 *= d__20;
/* Computing 3rd power */
    d__21 = *z__;
/* Computing 2nd power */
    d__22 = *z__;
    p1rc[16] = p1c[1029] * (d__2 * (d__1 * d__1)) + p1c[1036] * (d__3 * d__3) 
	    * *z__ + p1c[1028] * (d__4 * d__4) + p1c[1043] * (d__5 * (d__5 * 
	    d__5)) * (d__6 * d__6) + p1c[1035] * (d__7 * (d__7 * d__7)) * *
	    z__ + p1c[1027] * (d__8 * (d__8 * d__8)) + p1c[1050] * (d__9 * 
	    d__9) * (d__10 * (d__10 * d__10)) + p1c[1042] * (d__11 * d__11) * 
	    (d__12 * d__12) + p1c[1034] * (d__13 * d__13) * *z__ + p1c[1026] *
	     (d__14 * d__14) + p1c[1057] * *y * (d__15 * d__15) + p1c[1049] * 
	    *y * (d__16 * (d__16 * d__16)) + p1c[1041] * *y * (d__17 * d__17) 
	    + p1c[1033] * *y * *z__ + p1c[1025] * *y + p1c[1064] * (d__19 * (
	    d__18 * d__18)) + p1c[1056] * (d__20 * d__20) + p1c[1048] * (
	    d__21 * (d__21 * d__21)) + p1c[1040] * (d__22 * d__22) + p1c[1032]
	     * *z__ + p1c[1024];
/* Computing 4th power */
    d__1 = *y, d__1 *= d__1;
/* Computing 3rd power */
    d__2 = *y;
/* Computing 3rd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *y;
/* Computing 2nd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *y;
/* Computing 2nd power */
    d__7 = *y;
/* Computing 3rd power */
    d__8 = *z__;
/* Computing 2nd power */
    d__9 = *z__;
/* Computing 4th power */
    d__10 = *z__, d__10 *= d__10;
/* Computing 3rd power */
    d__11 = *z__;
/* Computing 2nd power */
    d__12 = *z__;
    p1rc[24] = p1c[1540] * (d__1 * d__1) + p1c[1547] * (d__2 * (d__2 * d__2)) 
	    * *z__ + p1c[1539] * (d__3 * (d__3 * d__3)) + p1c[1554] * (d__4 * 
	    d__4) * (d__5 * d__5) + p1c[1546] * (d__6 * d__6) * *z__ + p1c[
	    1538] * (d__7 * d__7) + p1c[1561] * *y * (d__8 * (d__8 * d__8)) + 
	    p1c[1553] * *y * (d__9 * d__9) + p1c[1545] * *y * *z__ + p1c[1537]
	     * *y + p1c[1568] * (d__10 * d__10) + p1c[1560] * (d__11 * (d__11 
	    * d__11)) + p1c[1552] * (d__12 * d__12) + p1c[1544] * *z__ + p1c[
	    1536];
/* Computing 3rd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *y;
/* Computing 2nd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *z__;
/* Computing 3rd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *z__;
    p1rc[32] = p1c[2051] * (d__1 * (d__1 * d__1)) + p1c[2058] * (d__2 * d__2) 
	    * *z__ + p1c[2050] * (d__3 * d__3) + p1c[2065] * *y * (d__4 * 
	    d__4) + p1c[2057] * *y * *z__ + p1c[2049] * *y + p1c[2072] * (
	    d__5 * (d__5 * d__5)) + p1c[2064] * (d__6 * d__6) + p1c[2056] * *
	    z__ + p1c[2048];
/* Computing 2nd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *z__;
    p1rc[40] = p1c[2562] * (d__1 * d__1) + p1c[2569] * *y * *z__ + p1c[2561] *
	     *y + p1c[2576] * (d__2 * d__2) + p1c[2568] * *z__ + p1c[2560];
    p1rc[48] = p1c[3073] * *y + p1c[3080] * *z__ + p1c[3072];
    p1rc[56] = p1c[3584];
/* Computing 6th power */
    d__1 = *y, d__1 *= d__1;
/* Computing 5th power */
    d__2 = *y, d__3 = d__2, d__2 *= d__2;
/* Computing 5th power */
    d__4 = *y, d__5 = d__4, d__4 *= d__4;
/* Computing 4th power */
    d__6 = *y, d__6 *= d__6;
/* Computing 2nd power */
    d__7 = *z__;
/* Computing 4th power */
    d__8 = *y, d__8 *= d__8;
/* Computing 4th power */
    d__9 = *y, d__9 *= d__9;
/* Computing 3rd power */
    d__10 = *y;
/* Computing 3rd power */
    d__11 = *z__;
/* Computing 3rd power */
    d__12 = *y;
/* Computing 2nd power */
    d__13 = *z__;
/* Computing 3rd power */
    d__14 = *y;
/* Computing 3rd power */
    d__15 = *y;
/* Computing 2nd power */
    d__16 = *y;
/* Computing 4th power */
    d__17 = *z__, d__17 *= d__17;
/* Computing 2nd power */
    d__18 = *y;
/* Computing 3rd power */
    d__19 = *z__;
/* Computing 2nd power */
    d__20 = *y;
/* Computing 2nd power */
    d__21 = *z__;
/* Computing 2nd power */
    d__22 = *y;
/* Computing 2nd power */
    d__23 = *y;
/* Computing 5th power */
    d__24 = *z__, d__25 = d__24, d__24 *= d__24;
/* Computing 4th power */
    d__26 = *z__, d__26 *= d__26;
/* Computing 3rd power */
    d__27 = *z__;
/* Computing 2nd power */
    d__28 = *z__;
/* Computing 6th power */
    d__29 = *z__, d__29 *= d__29;
/* Computing 5th power */
    d__30 = *z__, d__31 = d__30, d__30 *= d__30;
/* Computing 4th power */
    d__32 = *z__, d__32 *= d__32;
/* Computing 3rd power */
    d__33 = *z__;
/* Computing 2nd power */
    d__34 = *z__;
    p1rc[1] = p1c[70] * (d__1 * (d__1 * d__1)) + p1c[77] * (d__3 * (d__2 * 
	    d__2)) * *z__ + p1c[69] * (d__5 * (d__4 * d__4)) + p1c[84] * (
	    d__6 * d__6) * (d__7 * d__7) + p1c[76] * (d__8 * d__8) * *z__ + 
	    p1c[68] * (d__9 * d__9) + p1c[91] * (d__10 * (d__10 * d__10)) * (
	    d__11 * (d__11 * d__11)) + p1c[83] * (d__12 * (d__12 * d__12)) * (
	    d__13 * d__13) + p1c[75] * (d__14 * (d__14 * d__14)) * *z__ + p1c[
	    67] * (d__15 * (d__15 * d__15)) + p1c[98] * (d__16 * d__16) * (
	    d__17 * d__17) + p1c[90] * (d__18 * d__18) * (d__19 * (d__19 * 
	    d__19)) + p1c[82] * (d__20 * d__20) * (d__21 * d__21) + p1c[74] * 
	    (d__22 * d__22) * *z__ + p1c[66] * (d__23 * d__23) + p1c[105] * *
	    y * (d__25 * (d__24 * d__24)) + p1c[97] * *y * (d__26 * d__26) + 
	    p1c[89] * *y * (d__27 * (d__27 * d__27)) + p1c[81] * *y * (d__28 *
	     d__28) + p1c[73] * *y * *z__ + p1c[65] * *y + p1c[112] * (d__29 *
	     (d__29 * d__29)) + p1c[104] * (d__31 * (d__30 * d__30)) + p1c[96]
	     * (d__32 * d__32) + p1c[88] * (d__33 * (d__33 * d__33)) + p1c[80]
	     * (d__34 * d__34) + p1c[72] * *z__ + p1c[64];
/* Computing 5th power */
    d__1 = *y, d__2 = d__1, d__1 *= d__1;
/* Computing 4th power */
    d__3 = *y, d__3 *= d__3;
/* Computing 4th power */
    d__4 = *y, d__4 *= d__4;
/* Computing 3rd power */
    d__5 = *y;
/* Computing 2nd power */
    d__6 = *z__;
/* Computing 3rd power */
    d__7 = *y;
/* Computing 3rd power */
    d__8 = *y;
/* Computing 2nd power */
    d__9 = *y;
/* Computing 3rd power */
    d__10 = *z__;
/* Computing 2nd power */
    d__11 = *y;
/* Computing 2nd power */
    d__12 = *z__;
/* Computing 2nd power */
    d__13 = *y;
/* Computing 2nd power */
    d__14 = *y;
/* Computing 4th power */
    d__15 = *z__, d__15 *= d__15;
/* Computing 3rd power */
    d__16 = *z__;
/* Computing 2nd power */
    d__17 = *z__;
/* Computing 5th power */
    d__18 = *z__, d__19 = d__18, d__18 *= d__18;
/* Computing 4th power */
    d__20 = *z__, d__20 *= d__20;
/* Computing 3rd power */
    d__21 = *z__;
/* Computing 2nd power */
    d__22 = *z__;
    p1rc[9] = p1c[581] * (d__2 * (d__1 * d__1)) + p1c[588] * (d__3 * d__3) * *
	    z__ + p1c[580] * (d__4 * d__4) + p1c[595] * (d__5 * (d__5 * d__5))
	     * (d__6 * d__6) + p1c[587] * (d__7 * (d__7 * d__7)) * *z__ + p1c[
	    579] * (d__8 * (d__8 * d__8)) + p1c[602] * (d__9 * d__9) * (d__10 
	    * (d__10 * d__10)) + p1c[594] * (d__11 * d__11) * (d__12 * d__12) 
	    + p1c[586] * (d__13 * d__13) * *z__ + p1c[578] * (d__14 * d__14) 
	    + p1c[609] * *y * (d__15 * d__15) + p1c[601] * *y * (d__16 * (
	    d__16 * d__16)) + p1c[593] * *y * (d__17 * d__17) + p1c[585] * *y 
	    * *z__ + p1c[577] * *y + p1c[616] * (d__19 * (d__18 * d__18)) + 
	    p1c[608] * (d__20 * d__20) + p1c[600] * (d__21 * (d__21 * d__21)) 
	    + p1c[592] * (d__22 * d__22) + p1c[584] * *z__ + p1c[576];
/* Computing 4th power */
    d__1 = *y, d__1 *= d__1;
/* Computing 3rd power */
    d__2 = *y;
/* Computing 3rd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *y;
/* Computing 2nd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *y;
/* Computing 2nd power */
    d__7 = *y;
/* Computing 3rd power */
    d__8 = *z__;
/* Computing 2nd power */
    d__9 = *z__;
/* Computing 4th power */
    d__10 = *z__, d__10 *= d__10;
/* Computing 3rd power */
    d__11 = *z__;
/* Computing 2nd power */
    d__12 = *z__;
    p1rc[17] = p1c[1092] * (d__1 * d__1) + p1c[1099] * (d__2 * (d__2 * d__2)) 
	    * *z__ + p1c[1091] * (d__3 * (d__3 * d__3)) + p1c[1106] * (d__4 * 
	    d__4) * (d__5 * d__5) + p1c[1098] * (d__6 * d__6) * *z__ + p1c[
	    1090] * (d__7 * d__7) + p1c[1113] * *y * (d__8 * (d__8 * d__8)) + 
	    p1c[1105] * *y * (d__9 * d__9) + p1c[1097] * *y * *z__ + p1c[1089]
	     * *y + p1c[1120] * (d__10 * d__10) + p1c[1112] * (d__11 * (d__11 
	    * d__11)) + p1c[1104] * (d__12 * d__12) + p1c[1096] * *z__ + p1c[
	    1088];
/* Computing 3rd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *y;
/* Computing 2nd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *z__;
/* Computing 3rd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *z__;
    p1rc[25] = p1c[1603] * (d__1 * (d__1 * d__1)) + p1c[1610] * (d__2 * d__2) 
	    * *z__ + p1c[1602] * (d__3 * d__3) + p1c[1617] * *y * (d__4 * 
	    d__4) + p1c[1609] * *y * *z__ + p1c[1601] * *y + p1c[1624] * (
	    d__5 * (d__5 * d__5)) + p1c[1616] * (d__6 * d__6) + p1c[1608] * *
	    z__ + p1c[1600];
/* Computing 2nd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *z__;
    p1rc[33] = p1c[2114] * (d__1 * d__1) + p1c[2121] * *y * *z__ + p1c[2113] *
	     *y + p1c[2128] * (d__2 * d__2) + p1c[2120] * *z__ + p1c[2112];
    p1rc[41] = p1c[2625] * *y + p1c[2632] * *z__ + p1c[2624];
    p1rc[49] = p1c[3136];
/* Computing 5th power */
    d__1 = *y, d__2 = d__1, d__1 *= d__1;
/* Computing 4th power */
    d__3 = *y, d__3 *= d__3;
/* Computing 4th power */
    d__4 = *y, d__4 *= d__4;
/* Computing 3rd power */
    d__5 = *y;
/* Computing 2nd power */
    d__6 = *z__;
/* Computing 3rd power */
    d__7 = *y;
/* Computing 3rd power */
    d__8 = *y;
/* Computing 2nd power */
    d__9 = *y;
/* Computing 3rd power */
    d__10 = *z__;
/* Computing 2nd power */
    d__11 = *y;
/* Computing 2nd power */
    d__12 = *z__;
/* Computing 2nd power */
    d__13 = *y;
/* Computing 2nd power */
    d__14 = *y;
/* Computing 4th power */
    d__15 = *z__, d__15 *= d__15;
/* Computing 3rd power */
    d__16 = *z__;
/* Computing 2nd power */
    d__17 = *z__;
/* Computing 5th power */
    d__18 = *z__, d__19 = d__18, d__18 *= d__18;
/* Computing 4th power */
    d__20 = *z__, d__20 *= d__20;
/* Computing 3rd power */
    d__21 = *z__;
/* Computing 2nd power */
    d__22 = *z__;
    p1rc[2] = p1c[133] * (d__2 * (d__1 * d__1)) + p1c[140] * (d__3 * d__3) * *
	    z__ + p1c[132] * (d__4 * d__4) + p1c[147] * (d__5 * (d__5 * d__5))
	     * (d__6 * d__6) + p1c[139] * (d__7 * (d__7 * d__7)) * *z__ + p1c[
	    131] * (d__8 * (d__8 * d__8)) + p1c[154] * (d__9 * d__9) * (d__10 
	    * (d__10 * d__10)) + p1c[146] * (d__11 * d__11) * (d__12 * d__12) 
	    + p1c[138] * (d__13 * d__13) * *z__ + p1c[130] * (d__14 * d__14) 
	    + p1c[161] * *y * (d__15 * d__15) + p1c[153] * *y * (d__16 * (
	    d__16 * d__16)) + p1c[145] * *y * (d__17 * d__17) + p1c[137] * *y 
	    * *z__ + p1c[129] * *y + p1c[168] * (d__19 * (d__18 * d__18)) + 
	    p1c[160] * (d__20 * d__20) + p1c[152] * (d__21 * (d__21 * d__21)) 
	    + p1c[144] * (d__22 * d__22) + p1c[136] * *z__ + p1c[128];
/* Computing 4th power */
    d__1 = *y, d__1 *= d__1;
/* Computing 3rd power */
    d__2 = *y;
/* Computing 3rd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *y;
/* Computing 2nd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *y;
/* Computing 2nd power */
    d__7 = *y;
/* Computing 3rd power */
    d__8 = *z__;
/* Computing 2nd power */
    d__9 = *z__;
/* Computing 4th power */
    d__10 = *z__, d__10 *= d__10;
/* Computing 3rd power */
    d__11 = *z__;
/* Computing 2nd power */
    d__12 = *z__;
    p1rc[10] = p1c[644] * (d__1 * d__1) + p1c[651] * (d__2 * (d__2 * d__2)) * 
	    *z__ + p1c[643] * (d__3 * (d__3 * d__3)) + p1c[658] * (d__4 * 
	    d__4) * (d__5 * d__5) + p1c[650] * (d__6 * d__6) * *z__ + p1c[642]
	     * (d__7 * d__7) + p1c[665] * *y * (d__8 * (d__8 * d__8)) + p1c[
	    657] * *y * (d__9 * d__9) + p1c[649] * *y * *z__ + p1c[641] * *y 
	    + p1c[672] * (d__10 * d__10) + p1c[664] * (d__11 * (d__11 * d__11)
	    ) + p1c[656] * (d__12 * d__12) + p1c[648] * *z__ + p1c[640];
/* Computing 3rd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *y;
/* Computing 2nd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *z__;
/* Computing 3rd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *z__;
    p1rc[18] = p1c[1155] * (d__1 * (d__1 * d__1)) + p1c[1162] * (d__2 * d__2) 
	    * *z__ + p1c[1154] * (d__3 * d__3) + p1c[1169] * *y * (d__4 * 
	    d__4) + p1c[1161] * *y * *z__ + p1c[1153] * *y + p1c[1176] * (
	    d__5 * (d__5 * d__5)) + p1c[1168] * (d__6 * d__6) + p1c[1160] * *
	    z__ + p1c[1152];
/* Computing 2nd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *z__;
    p1rc[26] = p1c[1666] * (d__1 * d__1) + p1c[1673] * *y * *z__ + p1c[1665] *
	     *y + p1c[1680] * (d__2 * d__2) + p1c[1672] * *z__ + p1c[1664];
    p1rc[34] = p1c[2177] * *y + p1c[2184] * *z__ + p1c[2176];
    p1rc[42] = p1c[2688];
/* Computing 4th power */
    d__1 = *y, d__1 *= d__1;
/* Computing 3rd power */
    d__2 = *y;
/* Computing 3rd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *y;
/* Computing 2nd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *y;
/* Computing 2nd power */
    d__7 = *y;
/* Computing 3rd power */
    d__8 = *z__;
/* Computing 2nd power */
    d__9 = *z__;
/* Computing 4th power */
    d__10 = *z__, d__10 *= d__10;
/* Computing 3rd power */
    d__11 = *z__;
/* Computing 2nd power */
    d__12 = *z__;
    p1rc[3] = p1c[196] * (d__1 * d__1) + p1c[203] * (d__2 * (d__2 * d__2)) * *
	    z__ + p1c[195] * (d__3 * (d__3 * d__3)) + p1c[210] * (d__4 * d__4)
	     * (d__5 * d__5) + p1c[202] * (d__6 * d__6) * *z__ + p1c[194] * (
	    d__7 * d__7) + p1c[217] * *y * (d__8 * (d__8 * d__8)) + p1c[209] *
	     *y * (d__9 * d__9) + p1c[201] * *y * *z__ + p1c[193] * *y + p1c[
	    224] * (d__10 * d__10) + p1c[216] * (d__11 * (d__11 * d__11)) + 
	    p1c[208] * (d__12 * d__12) + p1c[200] * *z__ + p1c[192];
/* Computing 3rd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *y;
/* Computing 2nd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *z__;
/* Computing 3rd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *z__;
    p1rc[11] = p1c[707] * (d__1 * (d__1 * d__1)) + p1c[714] * (d__2 * d__2) * 
	    *z__ + p1c[706] * (d__3 * d__3) + p1c[721] * *y * (d__4 * d__4) + 
	    p1c[713] * *y * *z__ + p1c[705] * *y + p1c[728] * (d__5 * (d__5 * 
	    d__5)) + p1c[720] * (d__6 * d__6) + p1c[712] * *z__ + p1c[704];
/* Computing 2nd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *z__;
    p1rc[19] = p1c[1218] * (d__1 * d__1) + p1c[1225] * *y * *z__ + p1c[1217] *
	     *y + p1c[1232] * (d__2 * d__2) + p1c[1224] * *z__ + p1c[1216];
    p1rc[27] = p1c[1729] * *y + p1c[1736] * *z__ + p1c[1728];
    p1rc[35] = p1c[2240];
/* Computing 3rd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *y;
/* Computing 2nd power */
    d__3 = *y;
/* Computing 2nd power */
    d__4 = *z__;
/* Computing 3rd power */
    d__5 = *z__;
/* Computing 2nd power */
    d__6 = *z__;
    p1rc[4] = p1c[259] * (d__1 * (d__1 * d__1)) + p1c[266] * (d__2 * d__2) * *
	    z__ + p1c[258] * (d__3 * d__3) + p1c[273] * *y * (d__4 * d__4) + 
	    p1c[265] * *y * *z__ + p1c[257] * *y + p1c[280] * (d__5 * (d__5 * 
	    d__5)) + p1c[272] * (d__6 * d__6) + p1c[264] * *z__ + p1c[256];
/* Computing 2nd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *z__;
    p1rc[12] = p1c[770] * (d__1 * d__1) + p1c[777] * *y * *z__ + p1c[769] * *
	    y + p1c[784] * (d__2 * d__2) + p1c[776] * *z__ + p1c[768];
    p1rc[20] = p1c[1281] * *y + p1c[1288] * *z__ + p1c[1280];
    p1rc[28] = p1c[1792];
/* Computing 2nd power */
    d__1 = *y;
/* Computing 2nd power */
    d__2 = *z__;
    p1rc[5] = p1c[322] * (d__1 * d__1) + p1c[329] * *y * *z__ + p1c[321] * *y 
	    + p1c[336] * (d__2 * d__2) + p1c[328] * *z__ + p1c[320];
    p1rc[13] = p1c[833] * *y + p1c[840] * *z__ + p1c[832];
    p1rc[21] = p1c[1344];
    p1rc[6] = p1c[385] * *y + p1c[392] * *z__ + p1c[384];
    p1rc[14] = p1c[896];
    p1rc[7] = p1c[448];
    return 0;
} /* subm17_ */

