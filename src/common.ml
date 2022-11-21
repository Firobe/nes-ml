type cli_flags = { uncap_speed : bool; save_mp4 : string option }
type set_pixel = x:int -> y:int -> color:Stdint.uint8 -> unit

exception End_of_movie
