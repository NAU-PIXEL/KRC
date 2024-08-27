# include <stdlib.h>

# include "latc8m_struct.h"

latc8m_struct wrap_latc8m_struct(int param_nwlat, f_real_array dtm4, f_real_array tst4, f_real_array tts4, f_real_array ttb4, f_real_array frost4, f_real_array afro4, f_real_array tta4, f_real_array ttx4, nd_real_array tmn4, nd_real_array tin, nd_real_array tax, nd_real_array tsf, nd_real_array tpf, f_int_array ndj4) {
  latc8m_struct wrapped_struct;
  wrapped_struct.param_nwlat = param_nwlat;
  wrapped_struct.dtm4 = dtm4;
  wrapped_struct.tst4 = tst4;
  wrapped_struct.tts4 = tts4;
  wrapped_struct.ttb4 = ttb4;
  wrapped_struct.frost4 = frost4;
  wrapped_struct.afro4 = afro4;
  wrapped_struct.tta4 = tta4;
  wrapped_struct.ttx4 = ttx4;
  wrapped_struct.tmn4 = tmn4;
  wrapped_struct.tin = tin;
  wrapped_struct.tax = tax;
  wrapped_struct.tsf = tsf;
  wrapped_struct.tpf = tpf;
  wrapped_struct.ndj4 = ndj4;
  return wrapped_struct;
}