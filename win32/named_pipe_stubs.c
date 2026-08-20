#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#if defined(_WIN32)

#include <assert.h>
#include <caml/osdeps.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <windows.h>

static value
retrieve_error_message (DWORD id)
{
  CAMLparam0 ();
  LPTSTR error = NULL;

  FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER |
                     FORMAT_MESSAGE_IGNORE_INSERTS,
                 NULL, id, MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                 (LPTSTR) &error, 0, NULL);

  LocalFree (error);
  CAMLreturn (caml_copy_string (error));
}

static void
raise_error (const char *name, DWORD id)
{
  CAMLparam0 ();
  CAMLlocal1 (res);

  res = caml_alloc_small (4, 0);
  Field (res, 0) = *caml_named_value ("Geneweb_named_pipe.Error");
  Field (res, 1) = caml_copy_string (name);
  Field (res, 2) = id;
  Field (res, 3) = retrieve_error_message (id);

  CAMLnoreturn;
}

static value
Val_open_mode (DWORD mode)
{
  switch (mode)
    {
    case PIPE_ACCESS_DUPLEX:
      return Val_int (0);
    case PIPE_ACCESS_INBOUND:
      return Val_int (1);
    case PIPE_ACCESS_OUTBOUND:
      return Val_int (2);
    case FILE_FLAG_FIRST_PIPE_INSTANCE:
      return Val_int (3);
    case FILE_FLAG_WRITE_THROUGH:
      return Val_int (4);
    case FILE_FLAG_OVERLAPPED:
      return Val_int (5);
    default:
      assert (false);
    }
}

static Open_mode_val (value v)
{
  switch (Int_val (v))
    {
    case 0:
      return PIPE_ACCESS_DUPLEX;
    case 1:
      return PIPE_ACCESS_INBOUND;
    case 2:
      return PIPE_ACCESS_OUTBOUND;
    case 3:
      return FILE_FLAG_FIRST_PIPE_INSTANCE;
    case 4:
      return FILE_FLAG_WRITE_THROUGH;
    case 5:
      return FILE_FLAG_OVERLAPPED;
    default:
      assert (false);
    }
}

static DWORD
flag_of_open_modes (value modes)
{
  value head = modes;
  DWORD flag = 0;

  while (head != Val_emptylist)
    {
      flag |= Dw_open_mode_val (Field (head, 0));
      head = Field (head, 1);
    }

  return flag;
}

static value
Val_pipe_mode (DWORD mode)
{
  switch (mode)
    {
    case PIPE_TYPE_BYTE:
      return Val_int (0);
    case PIPE_TYPE_MESSAGE:
      return Val_int (1);
    case PIPE_READMODE_BYTE:
      return Val_int (2);
    case PIPE_READMODE_MESSAGE:
      return Val_int (3);
    case PIPE_WAIT:
      return Val_int (4);
    case PIPE_NOWAIT:
      return Val_int (5);
    case PIPE_ACCEPT_REMOTE_CLIENTS:
      return Val_int (6);
    case PIPE_REJECT_REMOTE_CLIENTS:
      return Val_int (7);
    default:
      assert (false);
    }
}

static DWORD
Pipe_mode_val (value v)
{
  switch (Int_val (v))
    {
    case 0:
      return PIPE_TYPE_BYTE;
    case 1:
      return PIPE_ACCESS_INBOUND;
    case 2:
      return PIPE_ACCESS_OUTBOUND;
    case 3:
      return FILE_FLAG_FIRST_PIPE_INSTANCE;
    case 4:
      return FILE_FLAG_WRITE_THROUGH;
    case 5:
      return FILE_FLAG_OVERLAPPED;
    default:
      assert (false);
    }
}

static DWORD
flag_of_pipe_modes (value modes)
{
  value head = modes;
  DWORD flag = 0;

  while (head != Val_emptylist)
    {
      flag |= Dw_pipe_mode_val (Field (head, 0));
      head = Field (head, 1);
    }

  return flag;
}

static value
Val_of_time_out (DWORD time_out)
{
  CAMLlocal1 (result);

  switch (time_out)
    {
    case NMPWAIT_USE_DEFAULT_WAIT:
      result = Val_int (0);
      break;
    case NMPWAIT_WAIT_FOREVER:
      result = Val_int (1);
      break;
    default:
      result = caml_alloc_small (1, 0);
      Field (result, 0) = Val_int (time_out);
    }

  CAMLreturn (result);
}

static DWORD
Time_out_of_val (value time_out)
{
  if (Is_long (time_out))
    {
      switch (Int_val (time_out))
        {
        case 0:
          return NMPWAIT_USE_DEFAULT_WAIT;
        case 1:
          return NMPWAIT_WAIT_FOREVER;
        default:
          assert (false);
        }
    }
  else
    {
      return Int_val (Field (result, 0));
    }
}

#endif // _WIN32

CAMLprim value
geneweb_win32_named_pipe_pipe_unlimited_instances (value unit)
{
  CAMLparam1 (unit);
#if defined(_WIN32)
  CAMLreturn (Val_int (PIPE_UNLIMITED_INSTANCES));
#else
  caml_invalid_argument ("pipe_unlimited_instances: not supported");
#endif
}

CAMLprim value
geneweb_win32_named_pipe_create_named_pipe (
    value name, value open_modes, value pipe_modes, value max_instances,
    value out_buffer_size, value in_buffer_size, value default_timeout)
{
#if defined(_WIN32)
  CAMLparam1 (name);

  wchar_t *wname = caml_stat_strdup_to_utf16 (String_val (name));

  caml_release_runtime_system ();
  // HANDLE handle =
  //     CreateNamedPipeW (wname, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE |
  //     PIPE_WAIT,
  //                       PIPE_UNLIMITED_INSTANCES, SIZEBUF, SIZEBUF, 0,
  //                       NULL);
  HANDLE h = CreateNamedPipeW (
      wname, flag_of_open_modes (open_modes), flag_of_pipe_modes (pipe_modes),
      PIPE_UNLIMITED_INSTANCES, Int_val (out_buffer_size),
      Int_val (in_buffer_size), Int_val (default_timeout), NULL);
  caml_acquire_runtime_system ();

  caml_stat_free (wname);

  if (h == INVALID_HANDLE_VALUE)
    raise_error ("CreateNamedPipeW", GetLastError ());

  CAMLreturn (caml_win32_alloc_handle (h));
#else
  caml_invalid_argument ("pipe_open: not supported");
#endif
}

CAMLprim value
geneweb_win32_named_pipe_connect_named_pipe (value handle)
{
#if defined(_WIN32)
  CAMLparam1 (handle);
  HANDLE h = Handle_val (handle);

  caml_release_runtime_system ();
  BOOL connected = ConnectNamedPipe (h, NULL);
  caml_acquire_runtime_system ();

  if (!connected)
    raise_error ("ConnectNamedPipe", GetLastError ());

  CAMLreturn (Val_unit);
#else
  caml_invalid_argument ("connect: not supported");
#endif
}

CAMLprim value
geneweb_win32_named_pipe_flush_all_buffers (value handle)
{
#if defined(_WIN32)
  CAMLparam1 (handle);
  HANDLE h = Handle_val (handle);
#else
  caml_invalid_argument ("flush_all_buffers: not supported");
#endif
}

CAMLprim value
geneweb_win32_named_pipe_disconnect_named_pipe (value handle)
{
#if defined(_WIN32)
  CAMLparam1 (handle);
  HANDLE h = Handle_val (handle);

  caml_release_runtime_system ();
  DisconnectNamedPipe (h);
  caml_acquire_runtime_system ();

  CAMLreturn (Val_unit);
#else
  caml_invalid_argument ("disconnect_named_pipe: not supported");
#endif
}

CAMLprim value
geneweb_win32_named_pipe_wait_named_pipe (value name, value time_out)
{
#if defined(_WIN32)
  CAMLparam2 (name, time_out);

  wchar_t *wname = caml_stat_strdup_to_utf16 (String_val (name));

  caml_release_runtime_system ();
  BOOL ready = WaitNamedPipeW (wname, Time_out_of_val (time_out));
  caml_acquire_runtime_system ();

  caml_stat_free (wname);

  if (!ready)
    raise_error ("WaitNamedPipeW", GetLastError ());

  CAMLreturn (Val_unit);
#else
  caml_invalid_argument ("wait_named_pipe: not supported");
#endif
}
