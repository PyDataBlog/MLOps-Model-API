#!/bin/bash -f
xv_path="/opt/Xilinx/Vivado/2016.2"
ExecStep()
{
"$@"
RETVAL=$?
if [ $RETVAL -ne 0 ]
then
exit $RETVAL
fi
}
ExecStep $xv_path/bin/xsim testbench_CORDIC_Arch2_behav -key {Behavioral:CORDIC_COS_32bits_round10:Functional:testbench_CORDIC_Arch2} -tclbatch testbench_CORDIC_Arch2.tcl -log simulate.log
