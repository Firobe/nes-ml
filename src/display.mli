(** SDL backend for displaying the emulator buffer and updating pixels *)

module type S = sig
  type t

  val init : unit -> unit
  (** Initialize the backend (must be called once) *)

  val exit : unit -> unit
  (** Release the backend *)

  val create :
    width:int ->
    height:int ->
    scale:int ->
    palette:int list ->
    ?vsync:bool ->
    ?save:string ->
    string ->
    t
  (** Create the emulation window with given attributes *)

  val delete : t -> unit

  val clear : t -> Stdint.uint8 -> unit
  (** Fill the buffer with the given color *)

  val get_window : t -> Tsdl.Sdl.window
  (** Retrieve SDL window *)

  val set_pixel : t -> x:int -> y:int -> color:Stdint.uint8 -> unit

  val render : ?after:(unit -> unit) -> t -> unit
  (** Refresh the displayed buffer *)
end

module Sdl_backend : S
module Headless_backend : S
