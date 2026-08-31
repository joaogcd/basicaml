val run
  :  ?input:(unit -> string option)
  -> ?output:(string -> unit)
  -> ?prompt:string option
  -> unit
  -> unit