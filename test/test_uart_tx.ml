open! Core
open Hardcaml
open Hardcaml_uart
open! Bits

let test ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~switch_every ~input =
  let module Uart_tx =
    Uart_tx.Make (struct
      (* This should trigger a switch every other cycle. *)
      let config =
        { Hardcaml_uart.Config.clock_frequency; baud_rate; include_parity_bit; stop_bits }
      ;;
    end)
  in
  let create_sim () =
    let module Sim = Cyclesim.With_interface (Uart_tx.I) (Uart_tx.O) in
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Uart_tx.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let sim = create_sim () in
  let inputs : _ Uart_tx.I.t = Cyclesim.inputs sim in
  let outputs : _ Uart_tx.O.t = Cyclesim.outputs sim in
  inputs.data_in_valid := of_int ~width:1 1;
  inputs.data_in := of_int ~width:8 input;
  Cyclesim.cycle sim;
  inputs.data_in_valid := of_int ~width:1 0;
  let rec loop_until_finished acc n =
    if n = 0
    then List.rev acc
    else (
      let rec wait_for n =
        if n = 0
        then ()
        else (
          Cyclesim.cycle sim;
          wait_for (n - 1))
      in
      let output = Bits.to_int !(outputs.uart_tx) in
      wait_for switch_every;
      loop_until_finished (output :: acc) (n - 1))
  in
  let output_bits = loop_until_finished [] 15 in
  print_s [%message (output_bits : int list)]
;;

let%expect_test "test" =
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:false
    ~stop_bits:1
    ~switch_every:2
    ~input:0b000_0000;
  [%expect {| (output_bits (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:false
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1;
  [%expect {| (output_bits (0 1 0 0 0 0 0 0 0 1 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:false
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1010_1010;
  [%expect {| (output_bits (0 0 1 0 1 0 1 0 1 1 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:false
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1111_1111;
  [%expect {| (output_bits (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:false
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1111_0000;
  [%expect {| (output_bits (0 0 0 0 0 1 1 1 1 1 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:1
    ~switch_every:2
    ~input:0b000_0000;
  [%expect {| (output_bits (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1;
  [%expect {| (output_bits (0 1 0 0 0 0 0 0 0 1 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1010_1010;
  [%expect {| (output_bits (0 0 1 0 1 0 1 0 1 0 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1111_1111;
  [%expect {| (output_bits (0 1 1 1 1 1 1 1 1 0 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:1
    ~switch_every:2
    ~input:0b1111_0000;
  [%expect {| (output_bits (0 0 0 0 0 1 1 1 1 0 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:2
    ~switch_every:2
    ~input:0b000_0000;
  [%expect {| (output_bits (0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:2
    ~switch_every:2
    ~input:0b1;
  [%expect {| (output_bits (0 1 0 0 0 0 0 0 0 1 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:2
    ~switch_every:2
    ~input:0b1010_1010;
  [%expect {| (output_bits (0 0 1 0 1 0 1 0 1 0 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:2
    ~switch_every:2
    ~input:0b1111_1111;
  [%expect {| (output_bits (0 1 1 1 1 1 1 1 1 0 1 1 1 1 1)) |}];
  test
    ~clock_frequency:200
    ~baud_rate:100
    ~include_parity_bit:true
    ~stop_bits:2
    ~switch_every:2
    ~input:0b1111_0000;
  [%expect {| (output_bits (0 0 0 0 0 1 1 1 1 0 1 1 1 1 1)) |}]
;;
