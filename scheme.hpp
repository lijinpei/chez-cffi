namespace chezscheme {
extern "C" {
#include "scheme.h"
constexpr char version[] = VERSION;
constexpr char machine_type[] = MACHINE_TYPE;
inline bool fixnump(ptr x) {
  return Sfixnump(x); 
}
inline bool charp(ptr x) {
  return Scharp(x); 
}
inline bool nullp(ptr x) {
  return Snullp(x); 
}
inline bool eof_objectp(ptr x) {
  return Seof_objectp(x); 
}
inline bool bwp_objectp(ptr x) {
  return Sbwp_objectp(x); 
}
inline bool booleanp(ptr x) {
  return Sbooleanp(x); 
}
inline bool pairp(ptr x) {
  return Spairp(x); 
}
inline bool symbolp(ptr x) {
  return Ssymbolp(x); 
}
inline bool procedurep(ptr x) {
  return Sprocedurep(x); 
}
inline bool flonump(ptr x) {
  return Sflonump(x); 
}
inline bool vectorp(ptr x) {
  return Svectorp(x); 
}
inline bool fxvectorp(ptr x) {
  return Sfxvectorp(x); 
}
inline bool bytevectorp(ptr x) {
  return Sbytevectorp(x); 
}
inline bool stringp(ptr x) {
  return Sstringp(x); 
}
inline bool bignump(ptr x) {
  return Sbignump(x); 
}
inline bool boxp(ptr x) {
  return Sboxp(x); 
}
inline bool inexactnump(ptr x) {
  return Sinexactnump(x); 
}
inline bool exactnump(ptr x) {
  return Sexactnump(x); 
}
inline bool ratnump(ptr x) {
  return Sratnump(x); 
}
inline bool inputportp(ptr x) {
  return Sinputportp(x); 
}
inline bool outputportp(ptr x) {
  return Soutputportp(x); 
}
inline bool recordp(ptr x) {
  return Srecordp(x); 
}
inline iptr fixnum_value(ptr x) {
  return Sfixnum_value(x); 
}
inline string_char char_value(ptr x) {
  return Schar_value(x); 
}
inline bool boolean_value(ptr x) {
  return Sboolean_value(x); 
}
inline ptr car(ptr x) {
  return Scar(x); 
}
inline ptr cdr(ptr x) {
  return Scdr(x); 
}
inline double flonum_value(ptr x) {
  return Sflonum_value(x); 
}
inline iptr vector_length(ptr x) {
  return Svector_length(x); 
}
inline iptr fxvector_length(ptr x) {
  return Sfxvector_length(x); 
}
inline iptr bytevector_length(ptr x) {
  return Sbytevector_length(x); 
}
inline octet * bytevector_data(ptr x) {
  return Sbytevector_data(x); 
}
inline iptr string_length(ptr x) {
  return Sstring_length(x); 
}
inline ptr unbox(ptr x) {
  return Sunbox(x); 
}
inline uptr unsigned_value(ptr x) {
  return Sunsigned_value(x); 
}
inline unsigned int unsigned32_value(ptr x) {
  return Sunsigned32_value(x); 
}
inline unsigned long unsigned64_value(ptr x) {
  return Sunsigned64_value(x); 
}
inline ptr sfixnum(int x) {
  return Sfixnum(x);
}
inline ptr schar(char x) {
  return Schar(x);
}
inline ptr sboolean(bool x) {
  return Sboolean(x);
}
const ptr snil = Snil;
const ptr sfalse = Sfalse;
const ptr strue = Strue;
#undef VERSION
#undef MACHINE_TYPE
#undef Sfixnump
#undef Scharp
#undef Snullp
#undef Seof_objectp
#undef Sbwp_objectp
#undef Sbooleanp
#undef Spairp
#undef Ssymbolp
#undef Sprocedurep
#undef Sflonump
#undef Svectorp
#undef Sfxvectorp
#undef Sbytevectorp
#undef Sstringp
#undef Sbignump
#undef Sboxp
#undef Sinexactnump
#undef Sexactnump
#undef Sratnump
#undef Sinputportp
#undef Soutputportp
#undef Srecordp
#undef Sfixnum
#undef Schar
#undef Sboolean
#undef Snil
#undef Sfalse
#undef Strue
#undef EXOPRT
#undef PROTO
} // namespace chezscheme
} // extern "C"
