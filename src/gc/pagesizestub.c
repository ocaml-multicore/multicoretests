#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <sysinfoapi.h>
#else
#include <unistd.h>
#endif

#include "caml/mlvalues.h"
#include "caml/memory.h"

CAMLprim value page_size(value ignored) {
  CAMLparam1(ignored);
  CAMLlocal1(result);

  long ps;
#ifdef _WIN32
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  ps = si.dwPageSize;
#else
  ps = sysconf(_SC_PAGESIZE); // page size in bytes
#endif

  result = Val_int(ps);
  CAMLreturn(result);
}
