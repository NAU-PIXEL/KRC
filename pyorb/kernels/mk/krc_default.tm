\begintext
#   Default metakernel for KRC, using latest versions of each kernel type.
#   Created 2025.11.20, NMS
\begindata
PATH_VALUES=(
    '/home/nsmith/KRC/pyorb/kernels',
)
PATH_SYMBOLS=(
    'k',
)
KERNELS_TO_LOAD=(
'$k/lsk/naif0012.tls',
'$k/pck/pck00010.tpc',
'$k/spk/20000001.bsp',
'$k/spk/20000623.bsp',
'$k/spk/de440.bsp',
'$k/spk/didymos_barycenter_s205_v01.bsp',
'$k/spk/didymos_system_s542_v01.bsp',
'$k/spk/mar099s.bsp',
)
