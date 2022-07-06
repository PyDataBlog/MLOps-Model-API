/**********************************************************************/
/*   ____  ____                                                       */
/*  /   /\/   /                                                       */
/* /___/  \  /                                                        */
/* \   \   \/                                                       */
/*  \   \        Copyright (c) 2003-2009 Xilinx, Inc.                */
/*  /   /          All Right Reserved.                                 */
/* /---/   /\                                                         */
/* \   \  /  \                                                      */
/*  \___\/\___\                                                    */
/***********************************************************************/

/* This file is designed for use with ISim build 0xcb73ee62 */

#define XSI_HIDE_SYMBOL_SPEC true
#include "xsi.h"
#include <memory.h>
#ifdef __GNUC__
#include <stdlib.h>
#else
#include <malloc.h>
#define alloca _alloca
#endif
static const char *ng0 = "D:/single_CPU/ALU_CTR.v";
static unsigned int ng1[] = {0U, 0U};
static unsigned int ng2[] = {2U, 0U};
static unsigned int ng3[] = {1U, 0U};
static unsigned int ng4[] = {6U, 0U};
static unsigned int ng5[] = {4U, 0U};
static unsigned int ng6[] = {5U, 0U};
static unsigned int ng7[] = {10U, 0U};
static unsigned int ng8[] = {7U, 0U};



static void Always_25_0(char *t0)
{
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    int t6;
    char *t7;
    char *t8;

LAB0:    t1 = (t0 + 1444U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(25, ng0);
    t2 = (t0 + 1640);
    *((int *)t2) = 1;
    t3 = (t0 + 1472);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(25, ng0);

LAB5:    xsi_set_current_line(26, ng0);
    t4 = (t0 + 600U);
    t5 = *((char **)t4);

LAB6:    t4 = ((char*)((ng1)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 2, t4, 2);
    if (t6 == 1)
        goto LAB7;

LAB8:    t2 = ((char*)((ng3)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 2, t2, 2);
    if (t6 == 1)
        goto LAB9;

LAB10:
LAB12:
LAB11:    xsi_set_current_line(29, ng0);

LAB16:    xsi_set_current_line(30, ng0);
    t2 = (t0 + 692U);
    t3 = *((char **)t2);

LAB17:    t2 = ((char*)((ng2)));
    t6 = xsi_vlog_unsigned_case_compare(t3, 4, t2, 4);
    if (t6 == 1)
        goto LAB18;

LAB19:    t2 = ((char*)((ng5)));
    t6 = xsi_vlog_unsigned_case_compare(t3, 4, t2, 4);
    if (t6 == 1)
        goto LAB20;

LAB21:    t2 = ((char*)((ng6)));
    t6 = xsi_vlog_unsigned_case_compare(t3, 4, t2, 4);
    if (t6 == 1)
        goto LAB22;

LAB23:    t2 = ((char*)((ng7)));
    t6 = xsi_vlog_unsigned_case_compare(t3, 4, t2, 4);
    if (t6 == 1)
        goto LAB24;

LAB25:
LAB27:
LAB26:    xsi_set_current_line(35, ng0);

LAB33:    xsi_set_current_line(35, ng0);
    t2 = ((char*)((ng2)));
    t4 = (t0 + 920);
    xsi_vlogvar_wait_assign_value(t4, t2, 0, 0, 3, 0LL);

LAB28:
LAB13:    goto LAB2;

LAB7:    xsi_set_current_line(27, ng0);

LAB14:    xsi_set_current_line(27, ng0);
    t7 = ((char*)((ng2)));
    t8 = (t0 + 920);
    xsi_vlogvar_wait_assign_value(t8, t7, 0, 0, 3, 0LL);
    goto LAB13;

LAB9:    xsi_set_current_line(28, ng0);

LAB15:    xsi_set_current_line(28, ng0);
    t3 = ((char*)((ng4)));
    t4 = (t0 + 920);
    xsi_vlogvar_wait_assign_value(t4, t3, 0, 0, 3, 0LL);
    goto LAB13;

LAB18:    xsi_set_current_line(31, ng0);

LAB29:    xsi_set_current_line(31, ng0);
    t4 = ((char*)((ng4)));
    t7 = (t0 + 920);
    xsi_vlogvar_wait_assign_value(t7, t4, 0, 0, 3, 0LL);
    goto LAB28;

LAB20:    xsi_set_current_line(32, ng0);

LAB30:    xsi_set_current_line(32, ng0);
    t4 = ((char*)((ng1)));
    t7 = (t0 + 920);
    xsi_vlogvar_wait_assign_value(t7, t4, 0, 0, 3, 0LL);
    goto LAB28;

LAB22:    xsi_set_current_line(33, ng0);

LAB31:    xsi_set_current_line(33, ng0);
    t4 = ((char*)((ng3)));
    t7 = (t0 + 920);
    xsi_vlogvar_wait_assign_value(t7, t4, 0, 0, 3, 0LL);
    goto LAB28;

LAB24:    xsi_set_current_line(34, ng0);

LAB32:    xsi_set_current_line(34, ng0);
    t4 = ((char*)((ng8)));
    t7 = (t0 + 920);
    xsi_vlogvar_wait_assign_value(t7, t4, 0, 0, 3, 0LL);
    goto LAB28;

}


extern void work_m_00000000002262663564_3418785828_init()
{
	static char *pe[] = {(void *)Always_25_0};
	xsi_register_didat("work_m_00000000002262663564_3418785828", "isim/test_isim_beh.exe.sim/work/m_00000000002262663564_3418785828.didat");
	xsi_register_executes(pe);
}
