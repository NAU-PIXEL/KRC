\begintext
#   Default metakernel for KRC, using latest versions of each kernel type.
#   Created 2025.11.20, NMS
\begindata
PATH_VALUES=(
    '/home/nsmith/KRC/kernels',
)
PATH_SYMBOLS=(
    'k',
)
KERNELS_TO_LOAD=(
'$k/lsk/naif0012.tls',
'$k/pck/pck00010.tpc',
'$k/spk/de440.bsp',
)