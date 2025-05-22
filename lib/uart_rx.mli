(** This implements an 8-bit UART receiver at a fixed baud rate given a
    fixed input frequency. The functor specifies whether to expect and validate
    a parity bit and a stop bit. *)
open! Core

open Hardcaml

module Make (_ : Config_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { data_out_valid : 'a
      ; data_out : 'a [@bits 8]
      ; parity_error : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
