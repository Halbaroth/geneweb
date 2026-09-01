#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>

#if defined(_WIN32)
#ifndef UNICODE
#define UNICODE 1
#endif
#include <assert.h>
#include <winsock2.h>
#include <windows.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/misc.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/intext.h>

#include <stdio.h>

static void dump (FILE *file, const unsigned char *data, size_t size) {
  for (size_t i = 0; i < size; i++) {
    if (i % 15 == 14)
      fprintf (file, "%02x\n", data[i]);
    else
      fprintf (file, "%02x ", data[i]);
  }
}

static value
Val_of_error (int e) {
  fprintf (stderr, "error %d\n", e);
  fflush (NULL);
  switch (e) {
    case WSANOTINITIALISED:
      return Val_int (0);
    case WSAENETDOWN:
      return Val_int (1);
    case WSAEINVAL:
      return Val_int (2);
    case WSAEINPROGRESS:
      return Val_int (3);
    case WSAEMFILE:
      return Val_int (4);
    case WSAENOBUFS:
      return Val_int (5);
    case WSAENOTSOCK:
      return Val_int (6);
    case WSAEFAULT:
      return Val_int (7);
    default:
      assert (FALSE);
      abort ();
  }
}

static value caml_alloc_ok (value payload) {
  CAMLparam1 (payload);
  CAMLlocal1 (result);
  result = caml_alloc (1, 0);
  Field (result, 0) = payload;
  CAMLreturn (result);
}

static value caml_alloc_error (value payload) {
  CAMLparam1 (payload);
  CAMLlocal1 (result);
  result = caml_alloc (1, 1);
  Field (result, 0) = payload;
  CAMLreturn (result);
}

// static value Val_of_protocol_info (LPWSAPROTOCOL_INFO pi) {
//   CAMLparam0 ();
//   CAMLlocal1 (result);
//   result = caml_alloc (1, Abstract_tag);
//   *((LPWSAPROTOCOL_INFO *) Data_abstract_val (result)) = pi;
//   CAMLreturn (result);
// }

static LPWSAPROTOCOL_INFO Protocol_info_val (value pi) {
  return (LPWSAPROTOCOL_INFO) Data_custom_val (pi);
}

CAMLexport const struct custom_fixed_length protocol_info_fixed_length = {
  .bsize_32 = sizeof (WSAPROTOCOL_INFO),
  .bsize_64 = sizeof (WSAPROTOCOL_INFO)
};

void protocol_info_serialize (value v, uintnat *bsize_32, uintnat *bsize_64) {
  caml_serialize_block_1 (Protocol_info_val (v), sizeof (WSAPROTOCOL_INFO));
  *bsize_32 = protocol_info_fixed_length.bsize_32;
  *bsize_64 = protocol_info_fixed_length.bsize_64;
}

uintnat protocol_info_deserialize (void *dst) {
#ifdef ARCH_SIXTYFOUR
  caml_deserialize_block_1 (dst, protocol_info_fixed_length.bsize_64);
  fprintf (stderr, "\n-------------\n");
  dump (stderr, (unsigned char *)dst, sizeof (WSAPROTOCOL_INFO));
  fprintf (stderr, "\n-------------\n");
  return protocol_info_fixed_length.bsize_64;
#else
  caml_deserialize_block_1 (dst, protocol_info_fixed_length.bsize_32);
  return protocol_info_fixed_length.bsize_32;
#endif
}

CAMLexport const struct custom_operations protocol_info_ops = {
  "geneweb_protocol_info",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  protocol_info_serialize,
  protocol_info_deserialize,
  custom_compare_ext_default,
  &protocol_info_fixed_length
};

static value alloc_protocol_info () {
  CAMLparam0 ();
  CAMLreturn (caml_alloc_custom (&protocol_info_ops, sizeof (WSAPROTOCOL_INFO), 0, 1));
}

// static LPWSAPROTOCOL_INFO alloc_protocol_info (void) {
//   return HeapAlloc (GetProcessHeap (), HEAP_ZERO_MEMORY, sizeof (WSAPROTOCOL_INFO));
// }
//
// static void free_protocol_info (LPWSAPROTOCOL_INFO pi) {
//   HeapFree (GetProcessHeap (), 0, pi);
// }

#endif // _WIN32

CAMLprim value
geneweb_win32_init (value unit) {
#if defined(_WIN32)
  CAMLparam1 (unit);
  caml_register_custom_operations (&protocol_info_ops);
  CAMLreturn (Val_unit);
#else
  caml_invalid_argument ("init: not supported");
#endif
}

CAMLprim value
geneweb_win32_duplicate_socket (value fd, value p) {
#if defined(_WIN32)
  CAMLparam2 (fd, p);
  CAMLlocal1 (pi);
  SOCKET socket;

  switch (Descr_kind_val (fd)) {
  case KIND_SOCKET:
    socket = Socket_val (fd);
    break;
  case KIND_HANDLE:
  default:
    caml_invalid_argument ("wsa_duplicate_socket: invalid file descriptor");
  }

  pi = alloc_protocol_info ();
  LPWSAPROTOCOL_INFO protocol_info = Protocol_info_val (pi);

  int pid = GetProcessId ((HANDLE) Long_val (p));
  fprintf (stderr, "PID in parent: %d\n", pid);
  fflush (NULL);

  // caml_release_runtime_system ();
  int e = WSADuplicateSocket (socket, pid, protocol_info);
  // caml_acquire_runtime_system ();

  dump (stderr, (unsigned char *)protocol_info, sizeof (*protocol_info));
  fflush (NULL);

  if (e != 0)
    CAMLreturn (caml_alloc_error (Val_of_error (WSAGetLastError ())));
  else
    CAMLreturn (caml_alloc_ok (pi));
#else
  caml_invalid_argument ("wsa_duplicate_socket: not supported");
#endif
}

static WSAPROTOCOL_INFO poo = { 0 };

CAMLprim value
geneweb_win32_protocol_info_to_socket (value cloexec, value pi) {
#if defined(_WIN32)
  fprintf (stderr, "SIZE = %lld\n", sizeof (WSAPROTOCOL_INFO));
  CAMLparam2 (cloexec, pi);
  DWORD flags = 0;

  fprintf (stderr, "PID in child: %ld\n", GetCurrentProcessId ());

  fprintf (stderr, "HERE\n");
  fflush (NULL);
  if (caml_unix_cloexec_p (cloexec))
    flags |= WSA_FLAG_NO_HANDLE_INHERIT;

  fprintf (stderr, "HERE 2\n");
  fflush (NULL);
  LPWSAPROTOCOL_INFO protocol_info = Protocol_info_val (pi);
  // LPWSAPROTOCOL_INFO copy = malloc (sizeof (WSAPROTOCOL_INFO));
  // fprintf (stderr, "PLOP: %lld\n", sizeof (WSAPROTOCOL_INFO));
  fflush (NULL);
  memcpy(&poo, protocol_info, sizeof (WSAPROTOCOL_INFO));
  fprintf (stderr, "HERE 3\n");
  fflush (NULL);
  // dump (stderr, (unsigned char *)protocol_info, sizeof (*protocol_info));
  fprintf (stderr, "\n-------------\n");
  fflush (NULL);

  // caml_release_runtime_system ();
  SOCKET s = WSASocket (FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, poo, 0, flags);
  // caml_acquire_runtime_system ();

  if (s == INVALID_SOCKET) {
    fprintf (stderr, "got error code %d\n", WSAGetLastError ());
    assert (FALSE);
  }

  CAMLreturn (caml_win32_alloc_socket (s));
#else
  caml_invalid_argument ("protocol_info_to_socket: not supported");
#endif
}

// CAMLprim value
// geneweb_win32_protocol_info_output (value oc, value pi) {
// #if defined(_WIN32)
//   CAMLparam2 (oc, pi);
//   struct channel *ch = Channel (oc);
//
//   CAMLreturn (Val_unit);
// #else
//   caml_invalid_argument ("output: not supported");
// #endif
// }
//
// CAMLprim value
// geneweb_win32_protocol_info_input (value ic) {
// #if defined(_WIN32)
//   CAMLparam1 (ic);
//   struct channel *ch = Channel (ic);
//
//   LPWSAPROTOCOL_INFO pi = alloc_protocol_info ();
//
//   caml_channel_lock (ch);
//
//   int sz = sizeof (*pi);
//   int read = 0;
//   while (read < sz)
//     read += caml_getblock (ch, (char *) (pi + read), sz - read);
//
//   caml_channel_unlock (ch);
//
//   CAMLreturn (Val_of_protocol_info (pi));
// #else
//   caml_invalid_argument ("input: not supported");
// #endif
// }
