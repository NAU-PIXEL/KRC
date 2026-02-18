# include "array_structs.h"

typedef struct latc8m_struct {
  int param_nwlat;

  f_real_array dtm4;
  f_real_array tst4;
  f_real_array tts4;
  f_real_array ttb4;
  f_real_array frost4;
  f_real_array afro4;
  f_real_array tta4;
  f_real_array ttx4;

  nd_real_array tmn4;
  nd_real_array tin;
  nd_real_array tax;
  nd_real_array tsf;
  nd_real_array tpf;

  f_int_array ndj4;

} latc8m_struct;