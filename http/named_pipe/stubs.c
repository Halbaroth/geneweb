#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>
#include <caml/osdeps.h>
#include <windows.h>
#define SIZEBUF 4096

static value retrieve_error_message (DWORD id) {
  CAMLparam0 ();
  LPTSTR error = NULL;

  FormatMessage (
    FORMAT_MESSAGE_FROM_SYSTEM | 
    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    id, 
    MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPTSTR) &error,
    0,
    NULL);

  LocalFree (error);
  CAMLreturn (caml_copy_string (error));
}

static void raise_error (const char *name, DWORD id) {
  CAMLparam0 ();
  CAMLlocal1 (res);
  
  res = caml_alloc_small (4, 0);
  Field (res, 0) = *caml_named_value ("Geneweb_named_pipe.Error");
  Field (res, 1) = caml_copy_string (name);
  Field (res, 2) = id;
  Field (res, 3) = retrieve_error_message (id);
  
  CAMLnoreturn;
}

CAMLprim value geneweb_named_pipe_open (value name) {
  CAMLparam1 (name);
  
  wchar_t *wname = caml_stat_strdup_to_utf16 (String_val (name));

  caml_release_runtime_system ();
  HANDLE handle =
    CreateNamedPipeW (wname, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_WAIT, 
      PIPE_UNLIMITED_INSTANCES, SIZEBUF, SIZEBUF, 0, NULL);
  caml_acquire_runtime_system ();
  
  caml_stat_free (wname);

  if (handle == INVALID_HANDLE_VALUE) 
    raise_error ("named_pipe_open", GetLastError ());

  CAMLreturn (caml_win32_alloc_handle (handle));
}

CAMLprim value geneweb_named_pipe_connect (value handle) {
  CAMLparam1 (handle);
  HANDLE h = Handle_val (handle);

  caml_release_runtime_system ();
  BOOL connected = ConnectNamedPipe (h, NULL);
  caml_acquire_runtime_system ();

  if (!connected)
    raise_error ("named_pipe_connect", GetLastError ());

  CAMLreturn (Val_unit);
}

// CAMLprim value geneweb_named_pipe_flush (value handle) {
//   CAMLparam1 (handle);
//   HANDLE h = Handle_val (handle);
// 
// }

CAMLprim value geneweb_named_pipe_disconnect (value handle) {
  CAMLparam1 (handle);
  HANDLE h = Handle_val (handle);

  caml_release_runtime_system ();
  DisconnectNamedPipe (h);
  caml_acquire_runtime_system ();

  CAMLreturn (Val_unit);
}

CAMLprim value geneweb_named_pipe_wait (value name) {
  CAMLparam1 (name);
  
  wchar_t *wname = caml_stat_strdup_to_utf16 (String_val (name));

  caml_release_runtime_system ();
  BOOL ready = WaitNamedPipeW (wname, NMPWAIT_USE_DEFAULT_WAIT);
  caml_acquire_runtime_system ();
  
  caml_stat_free (wname);

  if (!ready)
    raise_error ("named_pipe_wait" , GetLastError ());

  CAMLreturn (Val_unit);
}
