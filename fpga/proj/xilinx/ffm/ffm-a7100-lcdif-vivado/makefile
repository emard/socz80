#vivado=/opt/Xilinx/Vivado/2015.3/bin/vivado
project=socz80_ffm_a7100
xc3sprog_interface = ft4232h_fast
#xc3sprog_interface = ft4232h
# name of resulting bitstream file (*.bit)
bitfile=$(project).runs/impl_1/socz80_ffm_a7100_lcdif.bit

junk=*~
junk+=.Xil vivado.log vivado.jou ip_upgrade.log
junk+=$(project).ip_user_files
junk+=$(project).sim
junk+=$(project).cache

include ../../include/vivado.mk
