(** This implements an 8-bit UART transmitter at a fixed baud rate given a
    fixed input frequency.

    Format: [start bit][data bits][parity bit][end bit(s)]

    At other times the UART line is held high. *)
open! Core

open Hardcaml

module Make (_ : Config_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { uart_tx : 'a
      ; data_in_ready : 'a
      ; idle : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
