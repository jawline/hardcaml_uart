open! Core
open Hardcaml
open Hardcaml_test_harness
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
  let module Harness = Cyclesim_harness.Make (Uart_tx.I) (Uart_tx.O) in
  Harness.run ~create:Uart_tx.hierarchical (fun ~inputs ~outputs sim ->
    inputs.data_in_valid := vdd;
    inputs.data_in := of_int_trunc ~width:8 input;
    Cyclesim.cycle sim;
    inputs.data_in_valid := gnd;
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
        let output = to_int_trunc !(outputs.uart_tx) in
        wait_for switch_every;
        loop_until_finished (output :: acc) (n - 1))
    in
    let output_bits = loop_until_finished [] 15 in
    print_s [%message (output_bits : int list)])
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
