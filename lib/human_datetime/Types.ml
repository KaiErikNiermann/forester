(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type void = |

type second = Second of int
type 'a minute = Minute of int * 'a option
type 'a hour = Hour of int * 'a option

type pm = Plus | Minus
type offset = Z | Offset of pm * void minute hour

type time_with_offset = second minute hour * offset

type day = Day of int * time_with_offset option
type month = Month of int * day option
type year = Year of int * month option

type datetime = year

let pp_void _fmt (x : void) =
  match x with
  | _ -> .

let pp_second fmt (Second s) =
  Format.fprintf fmt "%02d" s

let pp_minute pp_rest fmt (Minute (m, rest_opt)) =
  Format.fprintf fmt "%02d" m;
  match rest_opt with
  | None -> ()
  | Some rest ->
    Format.fprintf fmt ":%a" pp_rest rest

let pp_hour pp_rest fmt (Hour (h, rest_opt)) =
  Format.fprintf fmt "%02d" h;
  match rest_opt with
  | None -> ()
  | Some rest ->
    Format.fprintf fmt ":%a" pp_rest rest

let pp_pm fmt = function
  | Plus -> Format.fprintf fmt "+"
  | Minus -> Format.fprintf fmt "-"

let pp_offset fmt = function
  | Z -> Format.fprintf fmt "Z"
  | Offset (pm, hm) ->
    pp_pm fmt pm;
    pp_hour (pp_minute pp_void) fmt hm

let pp_time_with_offset fmt (hms, offset) =
  pp_hour (pp_minute pp_second) fmt hms;
  pp_offset fmt offset

let pp_day fmt (Day (d, time_with_offset_opt)) =
  Format.fprintf fmt "%02d" d;
  match time_with_offset_opt with
  | None -> ()
  | Some time_with_offset ->
    Format.fprintf fmt "T%a" pp_time_with_offset time_with_offset

let pp_month fmt (Month (m, day_opt)) =
  Format.fprintf fmt "%02d" m;
  match day_opt with
  | None -> ()
  | Some day ->
    Format.fprintf fmt "-%a" pp_day day

let pp_year fmt (Year (y, month_opt)) =
  Format.fprintf fmt "%04d" y;
  match month_opt with
  | None -> ()
  | Some month ->
    Format.fprintf fmt "-%a" pp_month month

let pp_datetime = pp_year

let zero_second = Second 0
let zero_minute = Minute (0, Some zero_second)
let zero_hms = Hour (0, Some zero_minute)
let zero_time_with_offset = (zero_hms, Z)
let zero_day = Day (1, Some zero_time_with_offset)

let zero_month = Month (1, Some zero_day)

let expand_minute (Minute (m, sec_opt)) =
  Minute (m, Option.some @@ Option.fold ~none: zero_second ~some: Fun.id sec_opt)

let expand_hour (Hour (h, min_opt)) =
  Hour (h, Option.some @@ Option.fold ~none: zero_minute ~some: expand_minute min_opt)

let expand_time_with_offset (hms, offset) = expand_hour hms, offset

let expand_day (Day (d, time_with_offset_opt)) =
  Day (d, Option.some @@ Option.fold ~none: zero_time_with_offset ~some: expand_time_with_offset time_with_offset_opt)

let expand_month (Month (m, day_opt)) =
  Month (m, Option.some @@ Option.fold ~none: zero_day ~some: expand_day day_opt)

let expand_year (Year (y, month_opt)) =
  Year (y, Option.some @@ Option.fold ~none: zero_month ~some: expand_month month_opt)

(* TODO *)
let pp_rfc_3399 fmt dt = pp_datetime fmt @@ expand_year dt
